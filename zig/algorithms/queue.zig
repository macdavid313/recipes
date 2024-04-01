const std = @import("std");
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

pub fn LinkedListQueue(comptime T: type) type {
    return struct {
        const Self = @This();
        const Node = struct {
            data: T,
            next: ?*Node,
        };

        head: ?*Node,
        tail: ?*Node,
        count: usize,
        allocator: Allocator,

        pub fn init(allocator: Allocator) Self {
            return Self{ .head = null, .tail = null, .count = 0, .allocator = allocator };
        }

        pub fn deinit(self: *Self) void {
            while (self.dequeue()) |_| {}
        }

        pub fn isEmpty(self: *Self) bool {
            return self.count == 0;
        }

        pub fn enqueue(self: *Self, element: T) !void {
            var new_node = try self.allocator.create(Node);
            new_node.data = element;
            new_node.next = null;

            if (self.isEmpty()) {
                self.head = new_node;
                self.tail = new_node;
            } else {
                self.tail.?.next = new_node;
                self.tail = new_node;
            }

            self.count += 1;
        }

        pub fn dequeue(self: *Self) ?T {
            const head = self.head orelse return null;
            defer self.allocator.destroy(head);
            self.count -= 1;
            self.head = head.next;
            return head.data;
        }

        pub fn top(self: *Self) ?T {
            return self.head.?.data;
        }

        pub fn size(self: *Self) usize {
            return self.count;
        }
    };
}

test "LinkedListQueue" {
    var q = LinkedListQueue(i32).init(std.testing.allocator);
    defer q.deinit();

    try expect(q.isEmpty());

    try q.enqueue(1);
    try q.enqueue(2);
    try q.enqueue(3);

    try expect(q.isEmpty() == false);
    try expect(q.size() == 3);

    try expect(q.dequeue() == 1);
    try expect(q.dequeue() == 2);
    try expect(q.top() == 3);
    try expect(q.size() == 1);

    try q.enqueue(4);
    try q.enqueue(5);

    try expect(q.top() == 3);
    try expect(q.dequeue() == 3);
    try expect(q.dequeue() == 4);
    try expect(q.dequeue() == 5);
    try expect(q.isEmpty());

    const arr = [_]i32{ 100, 99, 98, 97, 96, 95 };
    for (arr) |item| {
        try q.enqueue(item);
    }
    try expect(q.size() == arr.len);
}
