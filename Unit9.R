#sports scheduling
aux = rep(0,24)

c1 = c(rep(1,4),rep(0,4),rep(0,4),rep(0,4),rep(0,4),rep(0,4)) #a plays b twice
c2 = c(rep(0,4),rep(0,4),rep(0,4),rep(0,4),rep(0,4),rep(1,4)) #c plays d twice
c3 = c(rep(0,4),rep(1,4),rep(0,4),rep(0,4),rep(0,4),rep(0,4))  #a plays c once
c4 = c(rep(0,4),rep(0,4),rep(1,4),rep(0,4),rep(0,4),rep(0,4))  #a plays d once
c5 = c(rep(0,4),rep(0,4),rep(0,4),rep(1,4),rep(0,4),rep(0,4))  #b plays c once
c6 = c(rep(0,4),rep(0,4),rep(0,4),rep(0,4),rep(1,4),rep(0,4))  #b plays d once

c7 = aux ; c7[c(1,5,9)] = 1  #a plays once in week 1
c8 = aux ; c8[c(2,6,19)] = 1  #a plays once in week 2
c9 = aux ; c9[c(3,7,11)] = 1  #a plays once in week 3
c10 = aux ; c10[c(4,8,12)] = 1  #a plays once in week 4

c11 = aux ; c11[c(1,13,17)] = 1  #abplays once in week 1
c12 = aux ; c12[c(1,13,17)+1] = 1  #b plays once in week 2
c13 = aux ; c13[c(1,13,17)+2] = 1  #b plays once in week 3
c14 = aux ; c14[c(1,13,17)+3] = 1  #b plays once in week 4

c15 = aux ; c15[c(5,13,21)] = 1  #c bplays once in week 1
c16 = aux ; c16[c(5,13,21)+1] = 1  #c plays once in week 2
c17 = aux ; c17[c(5,13,21)+2] = 1  #c plays once in week 3
c18 = aux ; c18[c(5,13,21)+3] = 1  #c plays once in week 4

c19 = aux ; c19[c(9,17,21)] = 1  #d bplays once in week 1
c20 = aux ; c20[c(9,17,21)+1] = 1  #d plays once in week 2
c21 = aux ; c21[c(9,17,21)+2] = 1  #d plays once in week 3
c22 = aux ; c22[c(9,17,21)+3] = 1  #d plays once in week 4

con=t(sapply(paste0('c',1:22),get))
rhs = c(2,2,rep(1,20))
dir=rep('==',22)
obj = c(1,2,4,8,rep(0,16),1,2,4,8)

sol=lp(direction='max',obj,con,rep("=",22),rhs,compute.sens=1,all.int=T)
sol

solg = Rglpk_solve_LP(obj,con,dir,rhs,types=rep('B',24),max=T,control=list("verbose" =T, "canonicalize_status" = F))
solg



#Mt. Sinai
day1 = c(1,1,1,1,1)
day2 = c(0,0,0,0,0)
c1 = c(day1,rep(day2,4)) 
c2 = c(day2,day1,rep(day2,3))
c3 = c(day2,day2,day1,day2,day2)
c4 = c(day2,day2,day2,day1,day2)
c5 = c(day2,day2,day2,day2,day1)
max1 = diag(25)
c16 = rep(c(1,0,0,0,0),5) 
c17 = rep(c(0,1,0,0,0),5) 
c18 = rep(c(0,0,1,0,0),5) 
c19 = rep(c(0,0,0,1,0),5) 
c20 = rep(c(0,0,0,0,1),5) 

rhs = c(10,10,10,10,10,
        2,3,0,1,6,2,3,1,1,6,2,3,0,1,6,2,3,1,1,6,2,3,0,1,6,
        rep(c(2,3,1,1,6),5),rep(0,25),
        3,12,2,2,18,6,18,3,4,25,
        39.4,117.4,19.9,26.3,189)

dir = c(rep("<=",5),rep('<=',25),rep('<=',25),rep('>=',25),
        rep('>=',5),rep("<=",5),rep("<=",5))

con = rbind(c1,c2,c3,c4,c5,max1,max1,max1,
            c16,c17,c18,c19,c20,c16,c17,c18,c19,c20,
            c1*8,c2*8,c3*8,c4*8,c5*8)

obj = rep(8/c(39.4,117.4,19.9,26.3,189),5)


solg = Rglpk_solve_LP(obj,con,dir,rhs,types=rep('B',25),max=T,control=list("verbose" =T, "canonicalize_status" = F))
solg


lp(direction='max',obj,con,dir,rhs,compute.sens=1,all.bin =T)
