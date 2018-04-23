pages 18 to 21 of Beal's master thesis

## Formal Automaton Description:

### Constants:

- `r_i`   = 0.05  (Radius of an inflection value)

- `p_s`   = 0.8   (Percent stimulus required to match a symbol)

- `t_c`   = 4     (Threshold where __uncertain__ lines become __certain__)

- `t_p`   = 6     (Threshold to prune certain lines)

- `n_w`   = 10000 (Number of comm lines)

- `n_wps` =   100 (Number of comm lines randomly selected for a new symbol)

- `w_m`   =    20 (Minimum number of comm lines per symbol)

### Input:

- `talk_in`(`F`)
   `F` is a set of (`s`,`i`), where `s`,`i` are symbols (feature lines)

- `listen_in`(`C_i`,`F`,`first`)
   `C_i`, 0 < `i` < `n_w`, in {1, 0, -1, X} (comm lines)
   `first` is a boolean, `F` as for `talk_in`

### Output:

- `talk_out`(`C_i`), `C_i` as for `listen_in`

- `listen_out`(`F`) as for `talk_in`

### States:

- `T_s` is a set of `x` = (`x_s`,`x_c`,`x_u`,`x_n`) where
   - `x_s` is a symbol
   - `x_c`,`x_u` are sets (initially empty) of `r` in N+, 0 < `r` <= `n_w`
   - `x_n` in N+

- `T_i` is a set (initially empty) of `y` = (`y_i`,`y_v`) where
   - `y_i` is a symbol
   - 0 <= `y_v` <= 1

- `c` is a set (initially empty) of (`l`,`v`) where
  - `l` in N+, 0 < `l` < `n_w`
  - `v` in {1,0,-1,X}
  - join rule is (`l`,1) U (`l`,-1) = (`l`,X) and (`l`,`X`) U (`l`,*) = (`l`,X)

- `f` is a set (initially empty) of (`s`,`i`) where `s`,`i` are symbols

- talking,listening are booleans, initially false

### Transitions:

- `talk_in`(`F`)

- `talk_out`(`C_i`)

- `listen_in`(`C_i`,`F`,`first`)

- `listen_out`(`F`)
