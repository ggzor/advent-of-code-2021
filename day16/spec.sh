# Part one
check -s '8A004A801A8002F478'              1  <<< 16
check -s '620080001611562C8802118E34'      1  <<< 12
check -s 'C0015000016115A2E0802F182340'    1  <<< 23
check -s 'A0016C880162017C3686B18A3D4780'  1  <<< 31
check input.txt 1 <<< 963

# Part two
check -s 'C200B40A82'                  2  <<< 3
check -s '04005AC33890'                2  <<< 54
check -s '880086C3E88112'              2  <<< 7
check -s 'CE00C43D881120'              2  <<< 9
check -s 'D8005AC2A8F0'                2  <<< 1
check -s 'F600BC2D8F'                  2  <<< 0
check -s '9C005AC2F8F0'                2  <<< 0
check -s '9C0141080250320F1802104A08'  2  <<< 1
check input.txt 2 <<< 1549026292886
