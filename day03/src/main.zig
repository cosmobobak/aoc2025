const std = @import("std");

pub fn main() !void {
    var r_buffer: [1024 * 64]u8 = undefined;
    var w_buffer: [1024 * 64]u8 = undefined;
    var stdin = std.fs.File.stdin().reader(&r_buffer);
    var stdout = std.fs.File.stdout().writer(&w_buffer);

    var r2_buffer: [1024 * 64]u8 = undefined;
    const bytes = try stdin.interface.readSliceShort(&r2_buffer);
    const input = r2_buffer[0..bytes];

    // iterate over lines of input
    var it = std.mem.tokenizeScalar(u8, input, '\n');
    var sum1: i64 = 0;
    var sum2: i64 = 0;
    while (it.next()) |x| {
        sum1 += find_max_arrangement(x, 2);
        sum2 += find_max_arrangement(x, 12);
    }

    try stdout.interface.print("sum1 = {}\n", .{sum1});
    try stdout.interface.print("sum2 = {}\n", .{sum2});

    try stdout.interface.flush(); // Must flush before reading!
}

fn find_max_arrangement(slice: []const u8, digits: usize) i64 {
    // in part 1, `digits` is fixed to 2, but now we generalise.
    var indices: [20]usize = undefined; // max 20 digits
    // the place in the slice we've used so far
    var used: usize = 0;
    for (0..digits) |i| {
        // we always wish to extract the maximal MSD first
        const max = std.mem.indexOfMax(u8, slice[used .. slice.len - digits + i + 1]) + used;
        indices[i] = max;
        used = max + 1;
    }
    // now compute the sum
    var sum: i64 = 0;
    for (0..digits) |i| {
        const idx = indices[i];
        sum += @as(i64, slice[idx] - '0') * std.math.pow(i64, 10, @as(i64, @intCast(digits)) - @as(i64, @intCast(i)) - 1);
    }
    return sum;
}
