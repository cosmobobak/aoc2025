import std/os/readline
import std/core/string

fun main() {
  var lines: list<list<char>> := Nil
  try { 
    while { True } {
      lines := Cons(readline().list, lines)
    }
  } fn(_) {}

  var grid_buf := lines.reverse.map fn(row) {
    row.vector
  }.vector

  // apparently we have to declare these outside
  // the loops, otherwise we get a very weird error:
  // > abstract type(s) escape(s) into the context
  // > term         : pickable_this_round := pickable_this_round + 1
  // > inferred type: () -> <local<$h>|_e> ()
  // > hint         : give a higher-rank type annotation to a function parameter?
  var picked_any: bool := True
  var pickable_so_far: int := 0
  var pickable_this_round: int := 0
  var adjacent: int := 0
  var to_replace: list<(int, int)> := Nil
  while { picked_any } {
    val grid = grid_buf
    picked_any := False
    pickable_this_round := 0
    adjacent := 0
    to_replace := Nil
    for (grid.length) fn(row) {
      for (grid[row].length) fn(col) {
        adjacent := 0
        for (-1, 1) fn(dr) {
          for (-1, 1) fn(dc) {
            if (dr != 0 || dc != 0) then {
              val r = row + dr
              val c = col + dc
              if (r >= 0
                && c >= 0
                && r < grid.length
                && c < grid[r].length) then {
                val at_loc = grid[r][c]
                if (at_loc == '@' || at_loc == '&') then {
                  adjacent := adjacent + 1
                }
              }
            }
          }
        }
        if (grid[row][col] == '@' && adjacent < 4) then {
          pickable_this_round := pickable_this_round + 1
          to_replace := Cons((row, col), to_replace)
        }
      }
    }

    println("Pickable rolls this round: " ++ pickable_this_round.show)

    // attempting to mutate a particular grid index breaks,
    // saying:
    // > grid_buf does not match the argument types
    // > context      :     grid_buf
    // > term         :     grid_buf
    // > inferred type: local-var<_h,vector<vector<char>>>
    // > expected type: local-var<_h2,vector<local-var<_h1,vector<_a>>>>
    // so we just recreate the whole grid:
    grid_buf := grid.list.map-indexed fn(r, row_vec) {
      row_vec.list.map-indexed fn(c, cell) {
        if (to_replace.any fn(pos) { pos == (r, c) }) then {
          '.'
        } else {
          cell
        }
      }.vector
    }.vector

    picked_any := (to_replace != Nil)

    pickable_so_far := pickable_so_far + pickable_this_round
  }

  println("Total pickable rolls: " ++ pickable_so_far.show)
}
