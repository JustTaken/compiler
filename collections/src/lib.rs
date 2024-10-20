use mem::{Arena, Container};
use util::{Index, Range};

pub struct Buffer<T> {
    ptr: *mut T,
}

pub struct Vector<T> {
    buffer: Buffer<T>,
    capacity: u32,
    len: u32,
}

pub struct RangeMap<T> {
    value: Buffer<T>,
    key: Buffer<Range>,
    capacity: u32,
}

impl<T: std::fmt::Debug> std::fmt::Debug for Vector<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("[\n")?;

        for element in self.offset(0) {
            f.write_fmt(format_args!(" {:?}\n", element))?;
        }

        f.write_str("]")
    }
}

impl<T> Buffer<T> {
    pub fn new(ptr: *mut T) -> Self {
        Self { ptr }
    }

    pub fn set(&mut self, item: T, index: usize) {
        unsafe { std::ptr::write(self.ptr.add(index), item) };
    }

    pub fn get_mut(&mut self, index: usize) -> &mut T {
        unsafe { &mut *self.ptr.add(index) }
    }

    pub fn get(&self, index: usize) -> &T {
        unsafe { &*self.ptr.add(index) }
    }

    pub fn at(&self, index: usize) -> T {
        unsafe { std::ptr::read(self.ptr.add(index)) }
    }

    pub fn slice_mut(&mut self, start: usize, end: usize) -> &mut [T] {
        unsafe { &mut *std::ptr::slice_from_raw_parts_mut(self.ptr.add(start), end - start) }
    }

    pub fn slice(&self, start: usize, end: usize) -> &[T] {
        unsafe { &*std::ptr::slice_from_raw_parts(self.ptr.add(start), end - start) }
    }

    pub fn copy(&mut self, from: *const T, start: usize, len: usize) {
        unsafe { std::ptr::copy(from, self.ptr.add(start), len) };
    }

    pub fn offset(&mut self, o: usize) -> *mut T {
        unsafe { self.ptr.add(o) }
    }

    pub fn fill(&mut self, len: usize, item: T)
    where
        T: Copy,
    {
        unsafe {
            for i in 0..len {
                std::ptr::write(self.ptr.add(i), item);
            }
        }
    }
}

impl<T> Vector<T> {
    pub fn new(capacity: usize, arena: &mut Arena) -> Self {
        Self {
            buffer: Buffer::new(arena.allocate(capacity)),
            capacity: capacity as u32,
            len: 0,
        }
    }

    pub fn push(&mut self, item: T) {
        if self.len >= self.capacity {
            panic!("out of length");
        }

        self.buffer.set(item, self.len as usize);
        self.len += 1;
    }

    pub fn extend(&mut self, items: &[T]) {
        let length = items.len() as u32;

        if self.len + length > self.capacity {
            panic!("out of length");
        }

        self.buffer
            .copy(items.as_ptr(), self.len as usize, length as usize);

        self.len += length;
    }

    pub fn mult_extend(&mut self, args: &[&[T]]) {
        for items in args {
            self.extend(items);
        }
    }

    pub fn extend_range(&mut self, items: &[T]) -> Range {
        let start = self.len;
        self.extend(items);

        Range::new(start, self.len)
    }

    pub fn self_extend(&mut self, to: u32, from: u32, len: u32) {
        self.buffer.copy(
            unsafe { self.buffer.ptr.add(from as usize) },
            to as usize,
            len as usize,
        );
    }

    pub fn pop(&mut self) -> T {
        let item = self.buffer.at(self.len as usize - 1);
        self.len -= 1;

        item
    }

    pub fn value(&self, at: u32) -> T {
        self.buffer.at(at as usize)
    }

    pub fn get_mut(&mut self, at: u32) -> &mut T {
        self.buffer.get_mut(at as usize)
    }

    pub fn get(&self, at: u32) -> &T {
        self.buffer.get(at as usize)
    }

    pub fn last_mut(&mut self) -> &mut T {
        self.buffer.get_mut(self.len as usize - 1)
    }

    pub fn last(&self) -> &T {
        self.buffer.get(self.len as usize - 1)
    }

    pub fn len(&self) -> u32 {
        self.len
    }

    pub fn set_len(&mut self, len: u32) {
        self.len = len;
    }

    pub fn range(&self, range: Range) -> &[T] {
        if range.start > range.end || range.end > self.len as Index {
            panic!("malformed range");
        }

        self.buffer.slice(range.start as usize, range.end as usize)
    }

    pub fn offset(&self, from: usize) -> &[T] {
        self.buffer.slice(from, self.len as usize)
    }

    pub fn buffer(&mut self) -> &mut [T] {
        self.buffer.slice_mut(0, self.capacity as usize)
    }

    pub fn pointer(&mut self, o: u32) -> *mut T {
        self.buffer.offset(o as usize)
    }

    pub fn clear(&mut self) {
        self.set_len(0);
    }
}

impl<T> RangeMap<T> {
    pub fn new(capacity: u32, arena: &mut Arena) -> Self {
        let mut key = Buffer::<Range>::new(arena.allocate(capacity as usize));
        key.fill(capacity as usize, Range::default());

        Self {
            value: Buffer::new(arena.allocate(capacity as usize)),
            key,
            capacity,
        }
    }

    pub fn push(&mut self, range: Range, item: T, buffer: &Vector<u8>) {
        let hash = util::hash(buffer.range(range));

        let mut code: u32 = hash % self.capacity;
        let mut count: u32 = 0;

        while self.key.at(code as usize).end > 0 && count < self.capacity {
            if code < self.capacity {
                code += 1;
            } else {
                code = 0;
            }

            count += 1;
        }

        self.value.set(item, code as usize);
        self.key.set(range, code as usize);
    }

    pub fn get(&self, range: Range, buffer: &Vector<u8>) -> Option<&T> {
        if let Some(i) = self.index(buffer.range(range), buffer) {
            Some(self.value.get(i as usize))
        } else {
            None
        }
    }

    pub fn index(&self, content: &[u8], buffer: &Vector<u8>) -> Option<u32> {
        let hash = util::hash(content);

        let mut code: u32 = hash % self.capacity;
        let mut count: u32 = 0;

        while self.key.at(code as usize).end > 0 && count < self.capacity {
            if util::compare(content, buffer.range(self.key.at(code as usize))) {
                return Some(code);
            }

            if code < self.capacity {
                code += 1;
            } else {
                code = 0;
            }

            count += 1;
        }

        None
    }

    pub fn addr_of(&mut self, content: &[u8], buffer: &Vector<u8>) -> Option<Container<T>> {
        if let Some(i) = self.index(content, buffer) {
            Some(Container::new(self.value.offset(i as usize)))
        } else {
            None
        }
    }

    pub fn clear(&mut self) {
        self.key.fill(self.capacity as usize, Range::default());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push() {
        let buffer = mem::allocate(1);
        let mut arena = Arena::new(buffer);

        let mut vec: Vector<u8> = Vector::new(256, &mut arena);

        for i in 0..256 {
            vec.push(i as u8);
        }

        assert_eq!(vec.value(255), 255);
    }
}
