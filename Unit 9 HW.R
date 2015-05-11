#unit 9 HW

obj = c(44.24236879,53.37919231,43.02117894,42.6068584,37.34498761,49.09506947,
        23.77686566,23.44540924,28.66584798,38.88067311,38.01059999,
        40.28936293,39.41928981,42.36096561,38.59064874,37.38911919)

c1 = c(rep(1,16))

rhs=c(10,10000000)

dir = c('<=','<=')

c2 = c(2925000.00 ,10000000.00 ,3750000.00 ,3500000.00 ,325000.00 ,
        8950000.00 ,1950000.00 ,1750000.00 ,4900000.00 ,1650000.00 ,
        1125000.00 ,2500000.00 ,1975000.00 ,3750000.00 ,1475000.00 ,
       750000.00 )

c3 = c(rep(0,9),rep(1,7))
con=rbind(c1,c2)


solg = Rglpk_solve_LP(obj,con,dir,rhs,types=rep('B',25),max=T,control=list("verbose" =T, "canonicalize_status" = F))
solg


solg1 = Rglpk_solve_LP(obj,rbind(con,c3),c(dir,'<='),c(rhs,2),types=rep('B',16),max=T,control=list("verbose" =T, "canonicalize_status" = F))
solg1


#lets try a different way

#we need 25 variables, as assignment per daay week to each dept

#we need 
# 5 constrains to make sure no more than 10 OR get assignd
# 5 constraints to make sure no dept gets more than its maximum hours
# 5 constraints to define that each det gets a minimum ORs per week
#5 constraints to define each dept gets no mroe than its maximum ORs 

#25 constr (per day per dept) to define that they get no more than their max
#25 to define they we never give them mroe than their teams
#they also added 25 constraints to a minimum, but all set to >=0 for now

#so we need a 95x25 matrix

con = matrix(0,nrow=95,ncol=25)
dir = rep("",95)
rhs = rep(0,95)
obj= rep(0,25)
types = rep('I',25)

#the order of vars is M=1-5, T=6-10, W=11-15, ....
#in each 5 opht, gyn, oral, oto, GS
days = 1:5
names(days) =c('M','T',"W","R",'F')

offset = 1:5
names(offset) = c('Opht','Gyn','Oral','Oto','Gral')


#add the max or per day 
c=0 # init complaint counter

i=1
for (day in days) {
  for (dept in offset) {
    con[day,i] = 1
    rhs[day] = 10
    dir[day] = '<='
    i=i+1
  }
  c=c+1
}

#max hour constraints
maxh = c(39.4,117.4,19.9,26.3,189)
for (dept in offset) {
  c=c+1
  for (day in days) {
    con[c,5*(day-1) +offset[dept]] = 8
    rhs[c] = maxh[dept]
    dir[c] = '<='
  }
}

#min or constraints
minor = c(3,12,2,2,18)
for (dept in offset) {
  c=c+1
  for (day in days) {
    con[c,5*(day-1) +offset[dept]] = 1
    rhs[c] = minor[dept]
    dir[c] = '>='
  }
}

#max or constraints
maxor = c(6,18,3,4,25)
for (dept in offset) {
  c=c+1
  for (day in days) {
    con[c,5*(day-1) +offset[dept]] = 1
    rhs[c] = maxor[dept]
    dir[c] = '<='
  }
}

#add the per pday per dept constraints
teams = c(2,3,0,1,6,2,3,1,1,6,2,3,0,1,6,2,3,1,1,6,2,3,0,1,6)
daymax = c(2,3,1,1,6,2,3,1,1,6,2,3,1,1,6,2,3,1,1,6,2,3,1,1,6)
daymin = rep(0,25)

for (x in 1:length(teams)) {
  c=c+1
  con[c,x] = 1
  rhs[c] = teams[x]
  dir[c] = '<='
}

for (x in 1:length(daymax)) {
  c=c+1
  con[c,x] = 1
  rhs[c] = daymax[x]
  dir[c] = '<='
}

for (x in 1:length(daymin)) {
  c=c+1
  con[c,x] = 1
  rhs[c] = daymin[x]
  dir[c] = '>='
}

obj= rep(8/maxh,5)


solg = Rglpk_solve_LP(obj,con,dir,rhs,types=types,max=T,control=list("verbose" =T, "canonicalize_status" = F))
solg


#now GS wants at least 4 per day
#m modify rhs to have elements for GS on the last 25 (each 5th ) be 4
rhs1= rhs
rhs1[c(75,80,85,90,95)] =4
solg1 = Rglpk_solve_LP(obj,con,dir,rhs1,types=types,max=T,control=list("verbose" =T, "canonicalize_status" = F))
solg1

matrix(solg1$solution,nrow=5,byrow=F,dimnames=list(names(offset),names(days)))



#PFIZER
#define tehsize

#88 variables, 22 brick 4 options each
#constraints
# 22 - one each for assigning each brick to 1 person
# 4 index constraints, one per person
# 4 disruption constraints
# we will define by person, by brikc the variables, 1 is person 1 at brick 1, 2 is person 1 at brick 2

centers = c(4,14,16,22)
N = matrix(c(1,1,1,0,0,0,0,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,
             1,1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1,
             1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,0,0,0,1,1,1,1,
             0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0),nrow=22,byrow=F)

indx=c(0.1609,0.1164,0.1026,0.1516,0.0939,0.132,0.0687,0.093,0.2116,0.2529,0.0868,0.0828,0.0975,0.8177,0.4115,0.3795,0.071,0.0427,0.1043,0.0997,0.1698,0.2531)

dist1 = matrix(c(16.16 ,19,25.29,0,3.07,1.22,2.8,2.87,3.8,12.35,11.11,21.99,8.82,7.93,9.34,8.31,7.31,7.55,11.13,17.49,11.03,36.12,
                 24.08,26.47,32.49,7.93,6.44,7.51,10.31,5.07,8.01,4.52,3.48,22.02,3.3,0,2.25,2.07,2.44,0.75,18.41,23.44,18.93,43.75,
                 24.32,27.24,33.42,8.31,7.56,8.19,10.95,5.67,7.41,4.35,2.97,24.07,5.36,2.07,1.11,0,1.11,1.53,19.26,24.76,19.28,44.43,
                 21.12,17.33,12.25,36.12,37.37,36.29,33.5,38.8,38.16,48.27,47.14,39.86,43.31,43.75,45.43,44.43,43.43,43.52,25.4,23.21,25.43,0),
               nrow=22,byrow=F)

dir =rep("",34)
rhs = rep(NA,34)
con = matrix(0,nrow=34,ncol=88)
# assign 1 brick to one person
c=1
for (i in 1:22) {
  aux = rep(0,22)
  aux[i] = 1
  con[i,] = rep(aux,4)
  dir[i] = "=="
  rhs[i] = 1
  c=c+1
}


#baalnce workload (<1.2 and >=0.8)
con[c,] = c(indx,rep(0,66))
con[c+1,] = c(rep(0,22), indx,rep(0,44))
con[c+2,] = c(rep(0,44), indx,rep(0,22))
con[c+3,] = c(rep(0,66), indx)
con[c+4,] = c(indx,rep(0,66))
con[c+5,] = c(rep(0,22), indx,rep(0,44))
con[c+6,] = c(rep(0,44), indx,rep(0,22))
con[c+7,] = c(rep(0,66), indx)

rhs[c:(c+7)] = c(rep(1.2,4),rep(0.8,4))
dir[c:(c+7)] = c(rep("<=",4),rep('>=',4))
c=c+8


#disruptin comstraints
con[c,] = c(N[,1],rep(0,66))
con[c+1,] = c(rep(0,22), N[,2],rep(0,44))
con[c+2,] = c(rep(0,44), N[,3],rep(0,22))
con[c+3,] = c(rep(0,66), N[,4])
rhs[c:(c+3)] = c(rep(2,4))
dir[c:(c+3)] = c(rep("<=",4))
c=c+4


#add obhective
obj = c(dist1[,1],dist1[,2],dist1[,3],dist1[,4])


solg = Rglpk_solve_LP(obj,con,dir,rhs,types=rep("B",88),max=F,control=list("verbose" =T, "canonicalize_status" = F))
solg
solm=matrix(solg$solution,nrow=22,byrow=F)
solg$optimum
sapply(1:4, function (x) sum(indx*solm[,x]))


rhs1=rhs
rhs1[23:30] = c(rep(1.1,4),rep(0.9,4))
solg1= Rglpk_solve_LP(obj,con,dir,rhs1,types=rep("B",88),max=F,control=list("verbose" =T, "canonicalize_status" = F))
solg1
solm1=matrix(solg1$solution,nrow=22,byrow=F)
solg1$optimum
sapply(1:4, function (x) sum(indx*solm1[,x]))


rhs2=rhs1
rhs2[31:34] = 3
solg2= Rglpk_solve_LP(obj,con,dir,rhs2,types=rep("B",88),max=F,control=list("verbose" =T, "canonicalize_status" = F))
solg2
solm2=matrix(solg2$solution,nrow=22,byrow=F)
solg2$optimum
sapply(1:4, function (x) sum(indx*solm1[,x]))


##### class assignment
class1 = c(1,1,2,1,1,2,1,2,1,1,1,2,1,1,1,2,1,1,1,1,2,1,2,1,2,2,2,1,2,1,2,1,2,1,2,2,1,2,2,2)
class2 = rep(0,40)
class2[class1==1]=2
class2[class1==2]=1
sum(class1==class2)

obj = c(class1,class2)
rhs= rep(0,42)
dir=rep("",42)
con=matrix(0,nrow=42,ncol=80)



for (i in 1:40) {
  aux = rep(0,40)
  aux[i] = 1
  con[i+2,] = c(aux,aux)
  rhs[i+2] = 1
  dir[i+2] = "=="
}

con[1,] = c(rep(1,40),rep(0,40))
con[2,]= c(rep(0,40),rep(1,40))
rhs[1:2] = c(20,20)
dir[1:2] = "=="


solg = Rglpk_solve_LP(obj,con,dir,rhs,types=rep("B",80),max=F,control=list("verbose" =T, "canonicalize_status" = F))
solg
solm=matrix(solg$solution,nrow=40,byrow=F)
solg$optimum



gender = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
gender1 = as.numeric(!gender)

con1 = rbind(con, c(rep(1,23),rep(0,17),rep(0,40)),c(rep(0,40),rep(1,23),rep(0,17)))
rhs1 = c(rhs,c(12,12))
dir1 = c(dir,"<=","<=")

solg1 = Rglpk_solve_LP(obj,con1,dir1,rhs1,types=rep("B",80),max=F,control=list("verbose" =T, "canonicalize_status" = F))
solg1
solm1=matrix(solg1$solution,nrow=40,byrow=F)
solg1$optimum
tapply(solm1[,1],gender1,sum)

twins = rep(0,40)
twins[10:11] = 1
con2 = rbind(con1,c(twins,rep(0,40)),c(rep(0,40),twins))
rhs2 = c(rhs1,1,1)
dir2=c(dir1,'==','==')

solg2 = Rglpk_solve_LP(obj,con2,dir2,rhs2,types=rep("B",80),max=F,control=list("verbose" =T, "canonicalize_status" = F))
solg2
solm2=matrix(solg2$solution,nrow=40,byrow=F)
solg2$optimum
tapply(solm2[,2],gender1,sum)

neighbors = rep(0,40)
neighbors[c(4,9,15,25,30)] = 1
con3 = rbind(con2,c(neighbors,rep(0,40)),c(rep(0,40),neighbors))
rhs3 = c(rhs2,2,2)
dir3=c(dir2,'>=','>=')

solg3 = Rglpk_solve_LP(obj,con3,dir3,rhs3,types=rep("B",80),max=F,control=list("verbose" =T, "canonicalize_status" = F))
solg3
solm3=matrix(solg3$solution,nrow=40,byrow=F)
solg3$optimum
tapply(solm3[,2],gender1,sum)


extra1 = rep(0,80)
extra1a = extra1
extra1a[c(20,61)] = c(1,1)
extra1b = extra1
extra1b[c(21,60)] = c(1,1)

extra2 = rep(0,80)
extra2[41]=1
extra3 = rep(0,80)
extra3[80]=1

con4 = rbind(con3,c(extra1a),c(extra1b),extra2,extra3)
rhs4 = c(rhs3,1,1,1,1)
dir4=c(dir3,rep('==',4))
solg4 = Rglpk_solve_LP(obj,con4,dir4,rhs4,types=rep("B",80),max=F,control=list("verbose" =T, "canonicalize_status" = F))
solg4$optimum
solm4=matrix(solg4$solution,nrow=40,byrow=F)
solm4
