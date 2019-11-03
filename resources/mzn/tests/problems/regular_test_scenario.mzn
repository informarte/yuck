% This test case generates a roster for a single worker.
% The task is to choose days off such that the resulting sequences of
% consecutive work days and of consecutive days off are not shorter
% and not longer than specified.

int: OFF = 1;
int: WORK = 2;

int: MINIMUM_NUMBER_OF_CONSECUTIVE_WORK_DAYS = 4;
int: MAXIMUM_NUMBER_OF_CONSECUTIVE_WORK_DAYS = 6;
int: MINIMUM_NUMBER_OF_CONSECUTIVE_DAYS_OFF = 1;
int: MAXIMUM_NUMBER_OF_CONSECUTIVE_DAYS_OFF = 3;

int: NUMBER_OF_WEEKS = 4;
int: PLANNING_HORIZON = NUMBER_OF_WEEKS * 7;

array [1..PLANNING_HORIZON] of var OFF..WORK: roster;

% number of inputs (off and work)
int: S = 2;
 % number of states
int: Q = 1 + MAXIMUM_NUMBER_OF_CONSECUTIVE_WORK_DAYS + MAXIMUM_NUMBER_OF_CONSECUTIVE_DAYS_OFF;
 % start state
int: q0 = 1;
% work states
% (w[n] means that the worker has been working for n days in a row.)
array [1..MAXIMUM_NUMBER_OF_CONSECUTIVE_WORK_DAYS] of int: w =
    (q0 + 1)..(q0 + MAXIMUM_NUMBER_OF_CONSECUTIVE_WORK_DAYS);
% off states
% (o[n] means that the worker has been resting for n days in a row.)
array [1..MAXIMUM_NUMBER_OF_CONSECUTIVE_DAYS_OFF] of int: o =
    (w[MAXIMUM_NUMBER_OF_CONSECUTIVE_WORK_DAYS] + 1)..Q;
 % acceptable states at the end of the roster
set of int: F = 1..Q;

/*
array [1..Q, 1..S] of 0..Q: d =
    [|o[1], w[1] | % q0
      0   , w[2] | % w[1]
      0   , w[3] | % w[2]
      0   , w[4] | % w[3]
      o[1], w[5] | % w[4] = w[MINIMUM_NUMBER_OF_CONSECUTIVE_WORK_DAYS]
      o[1], w[6] | % w[5]
      o[1], 0    | % w[6] = w[MAXIMUM_NUMBER_OF_CONSECUTIVE_WORK_DAYS]
      o[2], w[1] | % o[1] = w[MINIMUM_NUMBER_OF_CONSECUTIVE_DAYS_OFF]
      o[3], w[1] | % o[2]
      0,    w[1]   % o[3] = w[MAXIMUM_NUMBER_OF_CONSECUTIVE_DAYS_OFF]
     |];
*/

function 0..Q: succ(1..Q: q, 1..S: s) =
    if q = q0
    then (if s = 1 then o[1] else w[1] endif)
    else
        if q < w[MINIMUM_NUMBER_OF_CONSECUTIVE_WORK_DAYS]
        then (if s = 1 then 0 else q + 1 endif)
        else
            if q < w[MAXIMUM_NUMBER_OF_CONSECUTIVE_WORK_DAYS]
            then (if s = 1 then o[1] else q + 1 endif)
            else
                if q = w[MAXIMUM_NUMBER_OF_CONSECUTIVE_WORK_DAYS]
                then (if s = 1 then o[1] else 0 endif)
                else
                    if q < o[MINIMUM_NUMBER_OF_CONSECUTIVE_DAYS_OFF]
                    then (if s = 1 then q + 1 else 0 endif)
                    else
                        if q < o[MAXIMUM_NUMBER_OF_CONSECUTIVE_DAYS_OFF]
                        then (if s = 1 then q + 1 else w[1] endif)
                        else (if s = 1 then 0 else w[1] endif)
                        endif
                    endif
                endif
            endif
        endif
    endif;

array [1..Q, 1..S] of 0..Q: d = array2d(1..Q, 1..S, [succ(q, s) | q in 1..Q, s in 1..S]);

output [if fix(roster[i]) = OFF then "- " else "* " endif | i in 1..PLANNING_HORIZON];
