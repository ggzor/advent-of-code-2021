# Part one
check test.txt  1 <<< 17
check input.txt 1 <<< 759

# Part two
check test.txt  2 << EOF
#####
#   #
#   #
#   #
#####
     
     
EOF

check input.txt 2 << EOF
#  # ####  ##  ###  #### #  # ###  ###  
#  # #    #  # #  #    # # #  #  # #  # 
#### ###  #    #  #   #  ##   #  # #  # 
#  # #    #    ###   #   # #  ###  ###  
#  # #    #  # # #  #    # #  #    # #  
#  # ####  ##  #  # #### #  # #    #  # 
EOF
