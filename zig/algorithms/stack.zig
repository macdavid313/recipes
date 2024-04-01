const std = @import("std");
const expect = std.testing.expect;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub fn LinkedListStack(comptime T: type) type {
    return struct {
        const Self = @This();
        const Node = struct { data: T, next: ?*Node = null };

        head: ?*Node,
        allocator: Allocator,
        count: usize,

        pub fn init(allocator: Allocator) Self {
            return Self{ .head = null, .allocator = allocator, .count = 0 };
        }

        pub fn deinit(self: *Self) void {
            while (self.pop()) |_| {}
        }

        pub fn isEmpty(self: *Self) bool {
            return self.count == 0;
        }

        pub fn push(self: *Self, element: T) !void {
            var new_node = try self.allocator.create(Node);
            new_node.data = element;
            new_node.next = self.head;
            self.head = new_node;
            self.count += 1;
        }

        pub fn pop(self: *Self) ?T {
            const poped: *Node = self.head orelse return null;
            defer self.allocator.destroy(poped);

            self.head = poped.next;
            self.count -= 1;
            return poped.data;
        }

        pub fn top(self: *Self) ?T {
            return self.head.?.data;
        }

        pub fn size(self: *Self) usize {
            return self.count;
        }
    };
}

test "Stack implemented by SinglyLinkedList" {
    var stack = LinkedListStack(f64).init(std.testing.allocator);
    defer stack.deinit();

    try expect(stack.isEmpty());

    try stack.push(3.0);
    try stack.push(2.0);
    try stack.push(1.0);

    try expect(stack.size() == 3);
    try expect(stack.isEmpty() == false);

    try expect(stack.top().? == 1.0);
    try expect(stack.pop().? == 1.0);
    try expect(stack.top().? == 2.0);
    try expect(stack.pop().? == 2.0);
    try expect(stack.top().? == 3.0);
    try expect(stack.pop().? == 3.0);

    try expect(stack.isEmpty());
    try expect(stack.pop() == null);
}

pub fn ArrayListStack(comptime T: type) type {
    return struct {
        const Self = @This();

        stack: ArrayList(T),

        pub fn init(allocator: Allocator) Self {
            return Self{ .stack = ArrayList(T).init(allocator) };
        }

        pub fn deinit(self: *Self) void {
            self.stack.deinit();
        }

        pub fn isEmpty(self: *Self) bool {
            return self.size() == 0;
        }

        pub fn push(self: *Self, val: T) !void {
            try self.stack.append(val);
        }

        pub fn pop(self: *Self) ?T {
            return self.stack.popOrNull();
        }

        pub fn top(self: *Self) ?T {
            const len = self.stack.items.len;
            if (len == 0) {
                return null;
            } else {
                return self.stack.items[len - 1];
            }
        }

        pub fn size(self: *Self) usize {
            return self.stack.items.len;
        }
    };
}

test "Stack implemented by ArrayList" {
    var stack = ArrayListStack(f64).init(std.testing.allocator);
    defer stack.deinit();

    try stack.push(3.0);
    try stack.push(2.0);
    try stack.push(1.0);

    try expect(stack.isEmpty() == false);

    try expect(stack.top().? == 1.0);
    try expect(stack.pop().? == 1.0);
    try expect(stack.top().? == 2.0);
    try expect(stack.pop().? == 2.0);
    try expect(stack.top().? == 3.0);
    try expect(stack.pop().? == 3.0);

    try expect(stack.isEmpty());
    try expect(stack.pop() == null);
}
