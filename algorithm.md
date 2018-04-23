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
   - Effect:
     - `talking` := `true`
     - for all (`s`,`i`) in `F`
       - if not exists `x` in `T_s` s.t. `x_s` = `s`
         - `T_s` := `T_s` U (`i`,`random`)
       - if not exists `y` in `T_i` s.t. `y_i` = `i`
         - `T_i` := `T_i` U (`i`,`random`)
       - let
         - `x` in `T_s` s.t. `x_s` = `s`
         - `y` in `T_i` s.t. `y_i` = `i`
         - `v_l` in {1,-1}, for all `l` in `x_c` s.t. precisely |`x_c`|*`y_v` of the set {`v_l`} are 1
         - for all `l` in `x_c`, `c` := `c` U (`l`, `v_l`)

- `talk_out`(`C_i`)
   - Preconditions:
     - for all (`l`,`v`) in `c`, `C_l` = `v`
     - for all `l` s.t. for all `v`, if (`l`,`v`) not in `c` then `C_l` = 0
     - talking := true
   - Effect:
     - talking := false

- `listen_in`(`C_i`,`F`,`first`)
   - Effect:
     - `listening` := `true`
     - for all `m` in `T_i`
       - if not `first` and exists (`s`,`i`) in `F` s.t. `s` = `m_s`
         - if `m_n` = 0
           - `m_n` := 1
           - `m_u` := {`j`|`C_j` != 0}
         - if `t_c` <= `m_n` < `t_p`
           - `m_c` := `m_c` U `m_u`
           - `m_u` := empty set
         - if `t_p` <= `m_n`
           - if |`m_c` /\ {`j`|`C_j` != 0}| >= `w_m`
             - `m_c` := `m_c` /\ {`j`| `C_j` != 0}
           - else
             - `T_s` := `T_s` - `m`
       - else
         - let
           - `x` = `m_c` /\ {`j` | `C_j` != 0}
           - `u` = |{`j` in `x` | C_i = 1 }|/{`j` in `x` | `C_j` in {1,-1}}|
           - if |`x` >= |`m_c`| * `p_s`
             - if exists `y` in `T_i` s.t. |`y_v` - `u`| < `r_i`
               - for all `y` in `T_i` s.t. |`y_v - u` < `r_i`
                 - `f` := `f` U (`m_s`,`y_i`)
             - else
               - `f` := `f` U (`m_s`,`null`)
     - if not `first`
       - for all `y` in `T_i`
         - if exists (`s`,`i`) in `F` s.t. `i` = `y_i` and exists `m` in `T_s` s.t `m_s` = `s`
           - let u = |{`j` in `m_c` | `C_k` = 1}|/|{`j` in `m_c` | `C_j` in {1,-1}}|
             - if |`u`-`y_v`| > `r_i`/2 and {`j` in `m_c` | `C_j` = X} = empty set
               - `y_v` := `u`
         - else
           - if exists (`s`,`i`) in `F`, `m` in `T_s`
             - s.t. |(|{`j` in `m_c` | `C_j`= 1}|/|{`j` in `m_c`|`C_j` in {1,-1}}|)-`y_v` < `r_i`
             - and {`j` in `m_c` | `C_j` = X} = empty set
             - `y_v` := `random`
       - for all `y` in `T_i`
         - if exists `z` in (`T_i` - `y`) s.t |`z_v` - `y_v` | < r_i*2
           - `T_i` := `T_i` - (random in {`y`,`z`})

- `listen_out`(`F`)
