use std::alloc::GlobalAlloc;
use std::alloc::Layout;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

pub struct TrackingAllocator<Allocator> {
    allocator: Allocator,
    usage: AtomicUsize,
    max_usage: AtomicUsize,
}

unsafe impl<Allocator: GlobalAlloc> GlobalAlloc for TrackingAllocator<Allocator> {
    unsafe fn alloc(&self, l: Layout) -> *mut u8 {
        let usage = self.usage.update_and_fetch(|usage| usage + l.size());
        self.max_usage.update_and_fetch(|max_usage| max_usage.max(usage));
        self.allocator.alloc(l)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, l: Layout) {
        self.allocator.dealloc(ptr, l);
        self.usage.fetch_sub(l.size(), Ordering::SeqCst);
    }
}

impl<Allocator> TrackingAllocator<Allocator> {
    pub const fn new(allocator: Allocator) -> Self {
        Self {
            allocator,
            usage: AtomicUsize::new(0),
            max_usage: AtomicUsize::new(0),
        }
    }

    pub fn max_usage(&self) -> usize {
        self.max_usage.load(Ordering::SeqCst)
    }
}

trait CasLoop {
    type T;
    fn fetch_and_update<F: Fn(Self::T) -> Self::T>(&self, f: F) -> Self::T;
    fn update_and_fetch<F: Fn(Self::T) -> Self::T>(&self, f: F) -> Self::T;
}

impl CasLoop for AtomicUsize {
    type T = usize;

    fn fetch_and_update<F: Fn(Self::T) -> Self::T>(&self, f: F) -> Self::T {
        let mut prev = self.load(Ordering::Relaxed);
        loop {
            let next = f(prev);
            match self.compare_exchange_weak(prev, next, Ordering::SeqCst, Ordering::Relaxed) {
                Ok(x) => return x,
                Err(next_prev) => prev = next_prev
            }
        }
    }

    fn update_and_fetch<F: Fn(Self::T) -> Self::T>(&self, f: F) -> Self::T {
        let mut prev = self.load(Ordering::Relaxed);
        loop {
            let next = f(prev);
            match self.compare_exchange_weak(prev, next, Ordering::SeqCst, Ordering::Relaxed) {
                Ok(_) => return next,
                Err(next_prev) => prev = next_prev
            }
        }
    }
}
