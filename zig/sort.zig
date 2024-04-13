const std = @import("std");
const expect = std.testing.expect;

fn swap(nums: []i32, i: usize, j: usize) void {
    if (i != j) {
        const tmp = nums[i];
        nums[i] = nums[j];
        nums[j] = tmp;
    }
    return;
}

pub fn selectionSort(nums: []i32) void {
    const n = nums.len;

    for (0..n - 2) |i| {
        var k = i;
        for (i + 1..n) |j| {
            if (nums[j] < nums[k]) {
                k = j;
            }
        }
        swap(nums, i, k);
    }
}

test "selection sort" {
    var actual = [_]i32{ 5, 4, 3, 2, 1 };
    const expected = [_]i32{ 1, 2, 3, 4, 5 };

    selectionSort(&actual);

    for (0..actual.len) |i| {
        try expect(actual[i] == expected[i]);
    }
}

pub fn bubbleSort(nums: []i32) void {
    var i: usize = nums.len - 1;
    var j: usize = undefined;

    while (i > 0) : (i -= 1) {
        j = 0;
        var flag = false;
        while (j < i) : (j += 1) {
            if (nums[j] > nums[j + 1]) {
                swap(nums, j, j + 1);
                flag = true;
            }
        }
        if (!flag) {
            break;
        }
    }
}

test "bubble sort" {
    var actual = [_]i32{ 5, 4, 3, 2, 1 };
    const expected = [_]i32{ 1, 2, 3, 4, 5 };

    bubbleSort(&actual);

    for (0..actual.len) |i| {
        try expect(actual[i] == expected[i]);
    }
}
