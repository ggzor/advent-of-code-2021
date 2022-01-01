# Part one
# check test.txt  1 <<< 0

# check <(cat <<EOF
# inp x
# mul x -1
# EOF
# ) 64 1 <<< -64

# check <(cat <<EOF
# inp z
# inp x
# mul z 3
# eql z x
# EOF
# ) 2,6 1 <<< 0

# check <(cat <<EOF
# inp w
# add z w
# mod z 2
# div w 2
# add y w
# mod y 2
# div w 2
# add x w
# mod x 2
# div w 2
# mod w 2
# EOF
# ) 15 1 <<< -64

check input.txt 1 <<< 29989297949519
check input.txt 2 <<< 19518121316118
