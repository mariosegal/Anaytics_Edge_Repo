#Use R to solve it AA problem
#

f.con=matrix(c(1,1,1,0,0,1),nrow=3,byrow=2)
f.dir=c('<=','<=','<=')
f.rhs = c(166,100,150)
f.obj= c(617,238)

sol = lp ("max", f.obj, f.con, f.dir, f.rhs,compute.sens = T)

sol$sens.coef.from
sol$sens.coef.to

#the sens provide the from to limits for the changes in the objective coefficients
#it is not the full sensitivity

#try othr package
library('linprog')

sol1 = solveLP( f.obj, f.rhs, f.con, maximum=TRUE,const.dir=f.dir )

#linprog is better
#########
f.rhs2 = c(166,166,0,0,0,0,0,0,80,120,75,100,60,100)
f.con2=matrix(c(1,1,1,1,0,0,
               1,1,0,0,1,1,
               1,0,0,0,0,0,
               0,1,0,0,0,0,
               0,0,1,0,0,0,
               0,0,0,1,0,0,
               0,0,0,0,1,0,
               0,0,0,0,0,1,
               1,0,0,0,0,0,
               0,1,0,0,0,0,
               0,0,1,0,0,0,
               0,0,0,1,0,0,
               0,0,0,0,1,0,
               0,0,0,0,0,1),nrow=14,byrow=2)

f.dir2=c('<=','<=','>=','>=','>=','>=','>=','>=','<=','<=','<=','<=','<=','<=')
f.obj2= c(428,190,642,224,512,190)
sol2 =  solveLP(f.obj2, f.rhs2, f.con2, maximum=TRUE,const.dir=f.dir2)

#Cancer

f.rhs3 = c(7,7,7,7,5,0,0,0,0,0,0)
f.con3 = matrix(c(2,0,0,0,1,0,
                  0,1,0,2,0,0,
                  0,0,1.5,1,0,0,
                  0,0,1.5,0,1,0,
                  0,2,0,0,2,0,
                  1,0,0,0,0,0,
                  0,1,0,0,0,0,
                  0,0,1,0,0,0,
                  0,0,0,1,0,0,
                  0,0,0,0,1,0,
                  0,0,0,0,0,1),nrow=11,byrow=T)
f.obj3 = c(3,4.5,2.5,1,2,4)
f.dir3 = c(rep('>=',4),'<=',rep('>=',6))
sol3 =  solveLP(f.obj3, f.rhs3, f.con3, maximum=F,const.dir=f.dir3)
sol3


#google
###################
#you do not need to add the slack constraints they
#are added automatically
f.rhs = c(170,100,160,140,80,80)
f.con = matrix(c(0.5,0.5,1.6,0,0,0,0,0,0,
                 0,0,0,1,.75,2,0,0,0,
                 0,0,0,0,0,0,.5,4,5,
                 1,0,0,1,0,0,1,0,0,
                 0,1,0,0,1,0,0,1,0,
                 0,0,1,0,0,1,0,0,1),nrow=6,byrow=T)
f.obj = c(0.5,0.5,1.6,1,0.75,2,0.5,4,5)
f.dir = c(rep('<=',6))
sol_google =  solveLP(f.obj, f.rhs, f.con, maximum=T,const.dir=f.dir)
sol_google






#what if ATT query CTR one went up to 0.5
#tricky because model doe snot use CTR
#that means that the pay per display goes to 2.5
f.con1 = f.con
f.con1[1,1] = 2.5
f.obj1 = f.obj
f.obj1[1] = 2.5
sol_google1 =  solveLP(f.obj1, f.rhs, f.con1, maximum=T,const.dir=f.dir)
sol_google1

#what if budget for ATT goes up to 200



#HW
f.obj_hw = c(-29.5,-26.31,-34.55,-15.23,-62.43,-26.68,-23.85,-31.66,37531.5)
f.con_hw = matrix(c(26.646,23.3832,31.542,12.4494,47.3481,22.2513,23.0943,27.3033,0,
                    1,0,0,0,0,0,0,0,0,
                    0,1,0,0,0,0,0,0,0,
                    0,0,1,0,0,0,0,0,0,
                    0,0,0,1,0,0,0,0,0,
                    0,0,0,0,1,0,0,0,0,
                    0,0,0,0,0,1,0,0,0,
                    0,0,0,0,0,0,1,0,0,
                    0,0,0,0,0,0,0,1,0,
                    0,0,0,0,0,0,0,0,1),nrow=10,byrow=T)
f.rhs_hw=c(10000,150,150,150,150,150,150,150,150,1)
f.dir_hw=c('=',rep('<=',8),'=')


hw1 = solveLP(f.obj_hw, f.rhs_hw, f.con_hw, maximum=T,const.dir=f.dir_hw,lpSolve=T)
hw1


#####
f.con_hw1 = matrix(c(26.646,23.3832,31.542,12.4494,47.3481,22.2513,23.0943,27.3033,0,
                    1,0,0,0,0,0,0,0,0,
                    0,1,0,0,0,0,0,0,0,
                    0,0,1,0,0,0,0,0,0,
                    0,0,0,1,0,0,0,0,0,
                    0,0,0,0,1,0,0,0,0,
                    0,0,0,0,0,1,0,0,0,
                    0,0,0,0,0,0,1,0,0,
                    0,0,0,0,0,0,0,1,0,
                    0,0,0,0,0,0,0,0,1,
                    1,0,0,0,0,0,0,0,0,
                    0,1,0,0,0,0,0,0,0,
                    0,0,1,0,0,0,0,0,0,
                    0,0,0,1,0,0,0,0,0,
                    0,0,0,0,1,0,0,0,0,
                    0,0,0,0,0,1,0,0,0,
                    0,0,0,0,0,0,1,0,0,
                    0,0,0,0,0,0,0,1,0),nrow=18,byrow=T)
f.rhs_hw1=c(10000,150,150,150,150,150,150,150,150,1,rep(75,8))
f.dir_hw1=c('=',rep('<=',8),'=',rep('<=',8))

hw1a = solveLP(f.obj_hw, f.rhs_hw1, f.con_hw1, maximum=T,const.dir=f.dir_hw1,lpSolve=T)
hw1a

f.rhs_hw2=c(10000,150,150,150,150,150,150,150,150,1,100,rep(75,7))
f.dir_hw2=c('=',rep('<=',8),'=',"=",rep('<=',7))

hw1b= solveLP(f.obj_hw, f.rhs_hw2, f.con_hw1, maximum=T,const.dir=f.dir_hw2,lpSolve=T)
hw1b

#this was cool, of course issue is not to optimize but to predict accurately the 
#future price, if that is wrog then poof
f.rhs_hw2 = c(2500,3000,2500,2600,2500,38000,2500,25000,26000,28000,28000,rep(0,26))
f.obj_hw2 = c(13.3,11.1,10.05,17.8,14.5,11.8,10.05,18.20,15.02,12.20,10.7,
              15,12.3,10.65,18.2,14.5,12.45,10.65,18.25,13.9,11.4,8.9,
              2025,14.4,11.5,10.15)

f.con_hw2 = matrix(c(0.400 , 0.375,	0.250,rep(0,23),
                     0,0,0,0.700 , 0.500,	0.350,	0.250,rep(0,19),
                     rep(0,7),0.675 , 0.450,	0.400,	0.250,rep(0,15),
                     rep(0,11),0.450 , 0.350,	0.200,rep(0,12),
                     rep(0,14),0.650 , 0.450,	0.400	,0.250,rep(0,8),
                     rep(0,18),0.625 , 0.500,	0.425,	0.425,rep(0,4),
                     rep(0,22),0.700 , 0.450,	0.350,	0.400,
                     0,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,
                     1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,
                     0,1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,
                     0,0,1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,
                     diag(26)),
                     nrow=37,byrow=T)
f.dir_hw2 = c(rep('<=',7),rep("=",4),rep('>=',26))
hw2= solveLP(f.obj_hw2, f.rhs_hw2, f.con_hw2, maximum=F,const.dir=f.dir_hw2,lpSolve=T)
hw2

yarn=c(2:4,1:4,1:4,2:4,1:4,1:4,1:4)
outs = c(2:4,1:4,1:4,2:4,1:4,0,0,0,0,1:4)
medium = sum(hw2$solution[c(2,6,10,13,17,25)])
tapply(hw2$solution,outs,sum)

f.rhs_hw2a = f.rhs_hw2
f.rhs_hw2a[6] = 38600
hw2a= solveLP(f.obj_hw2, f.rhs_hw2a, f.con_hw2, maximum=F,const.dir=f.dir_hw2,lpSolve=T)
hw2a
hw2$opt - hw2a$opt

f.rhs_hw2b =  c(2500,3000,2500,2600,2500,38000,2500,25000,26000,28000,28000,600,rep(0,27))
f.obj_hw2b = c(f.obj_hw2,5.70)
f.con_hw2b = matrix(c(0.400 , 0.375,  0.250,rep(0,24),
                     0,0,0,0.700 , 0.500,	0.350,	0.250,rep(0,20),
                     rep(0,7),0.675 , 0.450,	0.400,	0.250,rep(0,16),
                     rep(0,11),0.450 , 0.350,	0.200,rep(0,13),
                     rep(0,14),0.650 , 0.450,	0.400	,0.250,rep(0,9),
                     rep(0,18),0.625 , 0.500,	0.425,	0.425,rep(0,5),
                     rep(0,22),0.700 , 0.450,	0.350,	0.400,0,
                     0,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,
                     1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,
                     0,1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,
                     0,0,1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,
                     rep(0,26),1,
                     diag(27)),
                   nrow=39,byrow=T)

f.dir_hw2b = c(rep('<=',7),rep("=",4),'<=',rep('>=',27))
hw2b= solveLP(f.obj_hw2b, f.rhs_hw2b, f.con_hw2b, maximum=F,
              const.dir=f.dir_hw2b,lpSolve=T)
hw2b
hw2$opt - 
  
     


f.rhs_hw2c[10] = 34000
hw2a= solveLP(f.obj_hw2, f.rhs_hw2a, f.con_hw2, maximum=F,const.dir=f.dir_hw2,lpSolve=T)
hw2a
hw2$opt - hw2a$opt


f.obj_hw2d= f.obj_hw2
f.obj_hw2d[19:22] = f.obj_hw2d[19:22]*0.95
hw2d= solveLP(f.obj_hw2d, f.rhs_hw2, f.con_hw2, maximum=F,const.dir=f.dir_hw2,lpSolve=T)
hw2d
hw2$opt - hw2d$opt



######
##GAS Blending
#######



#constraints
f.rhs3 = c(5000,5000,5000,14000,0,0,0,0,0,0)
f.con3 = matrix(c(1,1,1,0,0,0,0,0,0,
                 0,0,0,1,1,1,0,0,0,
                 0,0,0,0,0,0,1,1,1,
                 1,1,1,1,1,1,1,1,1,
                 2,0,0,-4,0,0,-2,0,0,
                 0,4,0,0,-2,0,0,0,0,
                 0,0,6,0,0,0,0,0,2,
                 -.5,0,0,1,0,0,2,0,0,
                 0,-1.5,0,0,0,0,0,1,0,
                 0,0,-.5,0,0,1,0,0,2),
               byrow=T,nrow=10)
f.dir3 = c(rep('<=',4),rep('>=',3),rep('<=',3))
f.obj3 = c(25,15,5,35,25,15,45,35,25)

hw3= solveLP(f.obj3, f.rhs3, f.con3, maximum=T,const.dir=f.dir3,lpSolve=F)
hw3
gas = c(rep(c(1,2,3),3))
tapply(hw3$solution,gas,sum)
oils=c(1,1,1,2,2,2,3,3,3)

#add demand
f.con.demand = matrix(c(1,0,0,1,0,0,1,0,0,
                        0,1,0,0,1,0,0,1,0,
                        0,0,1,0,0,1,0,0,1),
                      nrow=3,byrow=T)
f.rhs.demand = c(3000,2000,1000)
f.dir.demand = rep('<=',3)
hw3a= solveLP(f.obj3, c(f.rhs3,f.rhs.demand), rbind(f.con3,f.con.demand), 
              maximum=T,const.dir=c(f.dir3,f.dir.demand),lpSolve=F,solve.dual=T)
hw3a
tapply(hw3a$solution,oils,sum)


#extra HW
