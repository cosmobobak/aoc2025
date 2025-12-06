const std = @import("std");

pub fn main() !void {
    var r_buffer: [1024 * 64]u8 = undefined;
    var w_buffer: [1024 * 64]u8 = undefined;
    var stdin = std.fs.File.stdin().reader(&r_buffer);
    var stdout = std.fs.File.stdout().writer(&w_buffer);

    var r2_buffer: [1024 * 64]u8 = undefined;
    const bytes = try stdin.interface.readSliceShort(&r2_buffer);
    const input = r2_buffer[0..bytes];

    try stdout.interface.print("Beginning work!\n", .{});

    // iterate over lines of input
    var it = std.mem.tokenizeScalar(u8, input, '\n');
    var sum: i64 = 0;
    while (it.next()) |x| {
        const max = std.mem.indexOfMax(u8, x[0 .. x.len - 1]);
        const next = std.mem.indexOfMax(u8, x[max + 1 ..]) + max + 1;
        const val = (x[max] - '0') * 10 + (x[next] - '0');
        try stdout.interface.print("val = {}\n", .{val});
        sum += val;
    }

    try stdout.interface.print("sum = {}\n", .{sum});

    try stdout.interface.flush(); // Must flush before reading!
}
