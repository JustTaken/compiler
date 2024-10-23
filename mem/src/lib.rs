use std::alloc::{self};

pub struct Container<T> {
    ptr: *mut T,
}

pub struct Arena {
    ptr: *mut u8,
    capacity: usize,
    len: usize,
}

impl<T> Container<T> {
    pub fn new(ptr: *mut T) -> Self {
        Self { ptr }
    }

    pub fn null() -> Self {
        return Self {
            ptr: std::ptr::null_mut(),
        };
    }

    pub fn is_some(&self) -> bool {
        !self.ptr.is_null()
    }

    pub fn set(&self, item: T) {
        unsafe { *self.ptr = item };
    }

    pub fn get(&self) -> &mut T {
        unsafe { &mut *self.ptr as &mut T }
    }

    pub fn pointer(&self) -> *mut T {
        self.ptr
    }

    pub fn eql(&self, other: &Container<T>) -> bool {
        self.ptr == other.ptr
    }
}

impl Arena {
    pub fn new(bytes: &mut [u8]) -> Arena {
        Arena {
            ptr: bytes.as_mut_ptr(),
            capacity: bytes.len(),
            len: 0,
        }
    }

    pub fn bytes(&mut self, size: usize) -> &mut [u8] {
        if size + self.len > self.capacity {
            panic!("out of bounds");
        }

        let ptr = unsafe { self.ptr.add(self.len) };

        self.len += size;

        unsafe { &mut *std::ptr::slice_from_raw_parts_mut(ptr, size) }
    }

    pub fn allocate<T>(&mut self, size: usize) -> *mut T {
        let length = std::mem::size_of::<T>() * size;

        if length + self.len > self.capacity {
            panic!("Allocation failed");
        }

        let mut ptr = unsafe { self.ptr.add(self.len) };
        self.len += length;

        let align = std::mem::align_of::<T>();
        let p: usize = unsafe { std::mem::transmute(ptr) };
        let rest = p % align;

        if rest > 0 {
            let offset = align - rest;
            self.len += offset;
            ptr = unsafe { ptr.add(offset) };
        }

        ptr as *mut T
    }

    pub fn create<T>(&mut self, item: T) -> Container<T> {
        let container = Container {
            ptr: self.allocate::<T>(1),
        };

        container.set(item);

        container
    }

    pub fn dealloc<T>(&mut self, count: usize) {
        self.len -= std::mem::size_of::<T>() * count;
    }

    pub fn print(&self) {
        println!("usage: {} of {}", self.len, self.capacity);
    }

    pub fn clear(&mut self) {
        self.len = 0;
    }
}

pub fn eql<T: Eq>(one: &[T], other: &[T]) -> bool {
    if one.len() < other.len() {
        return false;
    }

    for i in 0..other.len() {
        if one[i] != other[i] {
            return false;
        }
    }

    true
}

const PAGE_SIZE: usize = 4096;
pub fn allocate(pages: usize) -> &'static mut [u8] {
    assert!(pages > 0, "cannot allocate zero bytes");

    let length = pages * PAGE_SIZE;

    let layout = alloc::Layout::array::<u8>(length).unwrap();
    let ptr = unsafe { alloc::alloc(layout) };

    if ptr.is_null() {
        alloc::handle_alloc_error(layout)
    }

    unsafe { &mut *std::ptr::slice_from_raw_parts_mut(ptr, length) }
}

pub fn deallocate(bytes: &'static mut [u8]) {
    let layout = alloc::Layout::array::<u8>(bytes.len()).unwrap();

    unsafe {
        alloc::dealloc(bytes.as_mut_ptr(), layout);
    }
}
