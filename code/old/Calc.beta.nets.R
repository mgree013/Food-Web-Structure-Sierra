library(betalink)
#Calculate beta diversity for all
#EVO
i.EVO.ELS2_1<-ToIgraph(EVO.ELS2_1)
plot.igraph(i.EVO.ELS2_1)
a.EVO.ELS2_1<-as_adjacency_matrix(i.EVO.ELS2_1,sparse=T)

i.EVO.ELS2_2<-ToIgraph(EVO.ELS2_2)
plot.igraph(i.EVO.ELS2_2)
a.EVO.ELS2_2<-as_adjacency_matrix(i.EVO.ELS2_2,sparse=T)

i.EVO.ELS2_3<-ToIgraph(EVO.ELS2_3)
plot.igraph(i.EVO.ELS2_3)
a.EVO.ELS2_3<-as_adjacency_matrix(i.EVO.ELS2_3,sparse=T)

i.EVO.ELS3_1<-ToIgraph(EVO.ELS3_1)
plot.igraph(i.EVO.ELS3_1)
a.EVO.ELS3_1<-as_adjacency_matrix(i.EVO.ELS3_1,sparse=T)

i.EVO.ELS3_2<-ToIgraph(EVO.ELS3_2)
plot.igraph(i.EVO.ELS3_2)
a.EVO.ELS3_2<-as_adjacency_matrix(i.EVO.ELS3_2,sparse=T)

i.EVO.ELS3_3<-ToIgraph(EVO.ELS3_3)
plot.igraph(i.EVO.ELS3_3)
a.EVO.ELS3_3<-as_adjacency_matrix(i.EVO.ELS3_3,sparse=T)

i.EVO.ELS4_1<-ToIgraph(EVO.ELS4_1)
plot.igraph(i.EVO.ELS4_1)
a.EVO.ELS4_1<-as_adjacency_matrix(i.EVO.ELS4_1,sparse=T)

i.EVO.ELS4_2<-ToIgraph(EVO.ELS4_2)
plot.igraph(i.EVO.ELS4_2)
a.EVO.ELS4_2<-as_adjacency_matrix(i.EVO.ELS4_2,sparse=T)

i.EVO.ELS4_3<-ToIgraph(EVO.ELS4_3)
plot.igraph(i.EVO.ELS4_3)
a.EVO.ELS4_3<-as_adjacency_matrix(i.EVO.ELS4_3,sparse=T)

i.EVO.ELS5_1<-ToIgraph(EVO.ELS5_1)
plot.igraph(i.EVO.ELS5_1)
a.EVO.ELS5_1<-as_adjacency_matrix(i.EVO.ELS5_1,sparse=T)

i.EVO.ELS5_2<-ToIgraph(EVO.ELS5_2)
plot.igraph(i.EVO.ELS5_2)
a.EVO.ELS5_2<-as_adjacency_matrix(i.EVO.ELS5_2,sparse=T)

i.EVO.ELS5_3<-ToIgraph(EVO.ELS5_3)
plot.igraph(i.EVO.ELS5_3)
a.EVO.ELS5_3<-as_adjacency_matrix(i.EVO.ELS5_3,sparse=T)

i.EVO.ELS6_1<-ToIgraph(EVO.ELS6_1)
plot.igraph(i.EVO.ELS6_1)
a.EVO.ELS6_1<-as_adjacency_matrix(i.EVO.ELS6_1,sparse=T,type="both")
aa.EVO.ELS6_1<-as.matrix(a.EVO.ELS6_1)

i.EVO.ELS6_2<-ToIgraph(EVO.ELS6_2)
plot.igraph(i.EVO.ELS6_2)
a.EVO.ELS6_2<-as_adjacency_matrix(i.EVO.ELS6_2,sparse=T)
aa.EVO.ELS6_2<-as.matrix(a.EVO.ELS6_2)

i.EVO.ELS6_3<-ToIgraph(EVO.ELS6_3)
plot.igraph(i.EVO.ELS6_3)
a.EVO.ELS6_3<-as_adjacency_matrix(i.EVO.ELS6_3,sparse=T)
aa.EVO.ELS6_3<-as.matrix(a.EVO.ELS6_3)

i.EVO.ELS7_1<-ToIgraph(EVO.ELS7_1)
plot.igraph(i.EVO.ELS7_1)
a.EVO.ELS7_1<-as_adjacency_matrix(i.EVO.ELS7_1,sparse=T)

i.EVO.ELS7_2<-ToIgraph(EVO.ELS7_2)
plot.igraph(i.EVO.ELS7_2)
a.EVO.ELS7_2<-as_adjacency_matrix(i.EVO.ELS7_2,sparse=T)

i.EVO.ELS7_3<-ToIgraph(EVO.ELS7_3)
plot.igraph(i.EVO.ELS7_3)
a.EVO.ELS7_3<-as_adjacency_matrix(i.EVO.ELS7_3,sparse=T)


i.EVO.ELS8_1<-ToIgraph(EVO.ELS8_1)
plot.igraph(i.EVO.ELS8_1)
a.EVO.ELS8_1<-as_adjacency_matrix(i.EVO.ELS8_1,sparse=T)

i.EVO.ELS8_2<-ToIgraph(EVO.ELS8_2)
plot.igraph(i.EVO.ELS8_2)
a.EVO.ELS8_2<-as_adjacency_matrix(i.EVO.ELS8_2,sparse=T)

i.EVO.ELS8_3<-ToIgraph(EVO.ELS8_3)
plot.igraph(i.EVO.ELS8_3)
a.EVO.ELS8_3<-as_adjacency_matrix(i.EVO.ELS8_3,sparse=T)

#Cascade
i.Cascade.CLS1_1<-ToIgraph(Cascade.CLS1_1)
plot.igraph(i.Cascade.CLS1_1)
a.Cascade.CLS1_1<-as_adjacency_matrix(i.Cascade.CLS1_1,sparse=T)

i.Cascade.CLS1_3<-ToIgraph(Cascade.CLS1_3)
plot.igraph(i.Cascade.CLS1_3)
a.Cascade.CLS1_3<-as_adjacency_matrix(i.Cascade.CLS1_3,sparse=T)

i.Cascade.CLS2_1<-ToIgraph(Cascade.CLS2_1)
plot.igraph(i.Cascade.CLS2_1)
a.Cascade.CLS2_1<-as_adjacency_matrix(i.Cascade.CLS2_1,sparse=T)

i.Cascade.CLS2_3<-ToIgraph(Cascade.CLS2_3)
plot.igraph(i.Cascade.CLS2_3)
a.Cascade.CLS2_3<-as_adjacency_matrix(i.Cascade.CLS2_3,sparse=T)

i.Cascade.CLS3_1<-ToIgraph(Cascade.CLS3_1)
plot.igraph(i.Cascade.CLS3_1)
a.Cascade.CLS3_1<-as_adjacency_matrix(i.Cascade.CLS3_1,sparse=T)

i.Cascade.CLS3_2<-ToIgraph(Cascade.CLS3_2)
plot.igraph(i.Cascade.CLS3_2)
a.Cascade.CLS3_2<-as_adjacency_matrix(i.Cascade.CLS3_2,sparse=T)

i.Cascade.CLS3_3<-ToIgraph(Cascade.CLS3_3)
plot.igraph(i.Cascade.CLS3_3)
a.Cascade.CLS3_3<-as_adjacency_matrix(i.Cascade.CLS3_3,sparse=T)

i.Cascade.CLS4_1<-ToIgraph(Cascade.CLS4_1)
plot.igraph(i.Cascade.CLS4_1)
a.Cascade.CLS4_1<-as_adjacency_matrix(i.Cascade.CLS4_1,sparse=T)

i.Cascade.CLS4_2<-ToIgraph(Cascade.CLS4_2)
plot.igraph(i.Cascade.CLS4_2)
a.Cascade.CLS4_2<-as_adjacency_matrix(i.Cascade.CLS4_2,sparse=T)

i.Cascade.CLS4_3<-ToIgraph(Cascade.CLS4_3)
plot.igraph(i.Cascade.CLS4_3)
a.Cascade.CLS4_3<-as_adjacency_matrix(i.Cascade.CLS4_3,sparse=T)

i.Cascade.CLS5_1<-ToIgraph(Cascade.CLS5_1)
plot.igraph(i.Cascade.CLS5_1)
a.Cascade.CLS5_1<-as_adjacency_matrix(i.Cascade.CLS5_1,sparse=T)

#RAE

i.RAE.RLS1_3<-ToIgraph(RAE.RLS1_3)
plot.igraph(i.RAE.RLS1_3)
a.RAE.RLS1_3<-as_adjacency_matrix(i.RAE.RLS1_3,sparse=T)

i.RAE.RLS2_1<-ToIgraph(RAE.RLS2_1)
plot.igraph(i.RAE.RLS2_1)
a.RAE.RLS2_1<-as_adjacency_matrix(i.RAE.RLS2_1,sparse=T)

i.RAE.RLS3_1<-ToIgraph(RAE.RLS3_1)
plot.igraph(i.RAE.RLS3_1)
a.RAE.RLS3_1<-as_adjacency_matrix(i.RAE.RLS3_1,sparse=T)

i.RAE.RLS3_2<-ToIgraph(RAE.RLS3_2)
plot.igraph(i.RAE.RLS3_2)
a.RAE.RLS3_2<-as_adjacency_matrix(i.RAE.RLS3_2,sparse=T)

i.RAE.RLS4_1<-ToIgraph(RAE.RLS4_1)
plot.igraph(i.RAE.RLS4_1)
a.RAE.RLS4_1<-as_adjacency_matrix(i.RAE.RLS4_1,sparse=T)

i.RAE.RLS4_2<-ToIgraph(RAE.RLS4_2)
plot.igraph(i.RAE.RLS4_2)
a.RAE.RLS4_2<-as_adjacency_matrix(i.RAE.RLS4_2,sparse=T)

i.RAE.RLS4_3<-ToIgraph(RAE.RLS4_3)
plot.igraph(i.RAE.RLS4_3)
a.RAE.RLS4_3<-as_adjacency_matrix(i.RAE.RLS4_3,sparse=T)

i.RAE.RLS5_1<-ToIgraph(RAE.RLS5_1)
plot.igraph(i.RAE.RLS5_1)
a.RAE.RLS5_1<-as_adjacency_matrix(i.RAE.RLS5_1,sparse=T)

i.RAE.RLS5_2<-ToIgraph(RAE.RLS5_2)
plot.igraph(i.RAE.RLS5_2)
a.RAE.RLS5_2<-as_adjacency_matrix(i.RAE.RLS5_2,sparse=T)

i.RAE.RLS5_3<-ToIgraph(RAE.RLS5_3)
plot.igraph(i.RAE.RLS5_3)
a.RAE.RLS5_3<-as_adjacency_matrix(i.RAE.RLS5_3,sparse=T)

i.RAE.RLS6_1<-ToIgraph(RAE.RLS6_1)
plot.igraph(i.RAE.RLS6_1)
a.RAE.RLS6_1<-as_adjacency_matrix(i.RAE.RLS6_1,sparse=T)

i.RAE.RLS6_2<-ToIgraph(RAE.RLS6_2)
plot.igraph(i.RAE.RLS6_2)
a.RAE.RLS6_2<-as_adjacency_matrix(i.RAE.RLS6_2,sparse=T)

#BUbbbs
i.BUBBS.Outlet.10477.trt.2003<-ToIgraph(BUBBS.Outlet.10477.trt.2003)
plot.igraph(i.BUBBS.Outlet.10477.trt.2003)
a.BUBBS.Outlet.10477.trt.2003<-as_adjacency_matrix(i.BUBBS.Outlet.10477.trt.2003,sparse=T)

i.BUBBS.Outlet.10477.trt.2004<-ToIgraph(BUBBS.Outlet.10477.trt.2004)
plot.igraph(i.BUBBS.Outlet.10477.trt.2004)
a.BUBBS.Outlet.10477.trt.2004<-as_adjacency_matrix(i.BUBBS.Outlet.10477.trt.2004,sparse=T)

i.BUBBS.Outlet.10477.trt.2011<-ToIgraph(BUBBS.Outlet.10477.trt.2011)
plot.igraph(i.BUBBS.Outlet.10477.trt.2011)
a.BUBBS.Outlet.10477.trt.2011<-as_adjacency_matrix(i.BUBBS.Outlet.10477.trt.2011,sparse=T)

i.BUBBS.Outlet.10487.trt.2003<-ToIgraph(BUBBS.Outlet.10487.trt.2003)
plot.igraph(i.BUBBS.Outlet.10487.trt.2003)
a.BUBBS.Outlet.10487.trt.2003<-as_adjacency_matrix(i.BUBBS.Outlet.10487.trt.2003,sparse=T)

i.BUBBS.Outlet.10487.trt.2004<-ToIgraph(BUBBS.Outlet.10487.trt.2004)
plot.igraph(i.BUBBS.Outlet.10487.trt.2004)
a.BUBBS.Outlet.10487.trt.2004<-as_adjacency_matrix(i.BUBBS.Outlet.10487.trt.2004,sparse=T)

i.BUBBS.Outlet.10487.trt.2011<-ToIgraph(BUBBS.Outlet.10487.trt.2011)
plot.igraph(i.BUBBS.Outlet.10487.trt.2011)
a.BUBBS.Outlet.10487.trt.2011<-as_adjacency_matrix(i.BUBBS.Outlet.10487.trt.2011,sparse=T)

i.BUBBS.Outlet.11007.fishless.2003<-ToIgraph(BUBBS.Outlet.11007.fishless.2003)
plot.igraph(i.BUBBS.Outlet.11007.fishless.2003)
a.BUBBS.Outlet.11007.fishless.2003<-as_adjacency_matrix(i.BUBBS.Outlet.11007.fishless.2003,sparse=T)

i.BUBBS.Outlet.11007.fishless.2004<-ToIgraph(BUBBS.Outlet.11007.fishless.2004)
plot.igraph(i.BUBBS.Outlet.11007.fishless.2004)
a.BUBBS.Outlet.11007.fishless.2004<-as_adjacency_matrix(i.BUBBS.Outlet.11007.fishless.2004,sparse=T)

i.BUBBS.Outlet.11007.fishless.2011<-ToIgraph(BUBBS.Outlet.11007.fishless.2011)
plot.igraph(i.BUBBS.Outlet.11007.fishless.2011)
a.BUBBS.Outlet.11007.fishless.2011<-as_adjacency_matrix(i.BUBBS.Outlet.11007.fishless.2011,sparse=T)

i.Bubbs.Outlet.10494.trt.2012<-ToIgraph(Bubbs.Outlet.10494.trt.2012)
plot.igraph(i.Bubbs.Outlet.10494.trt.2012)
a.Bubbs.Outlet.10494.trt.2012<-as_adjacency_matrix(i.Bubbs.Outlet.10494.trt.2012,sparse=T)

i.Bubbs.Outlet.Vidette.below.2003<-ToIgraph(Bubbs.Outlet.Vidette.below.2003)
plot.igraph(i.Bubbs.Outlet.Vidette.below.2003)
a.Bubbs.Outlet.Vidette.below.2003<-as_adjacency_matrix(i.Bubbs.Outlet.Vidette.below.2003,sparse=T)

i.Bubbs.Outlet.Vidette.below.2004<-ToIgraph(Bubbs.Outlet.Vidette.below.2004)
plot.igraph(i.Bubbs.Outlet.Vidette.below.2004)
a.Bubbs.Outlet.Vidette.below.2004<-as_adjacency_matrix(i.Bubbs.Outlet.Vidette.below.2004,sparse=T)

i.Bubbs.Outlet.Vidette.below.2011<-ToIgraph(Bubbs.Outlet.Vidette.below.2011)
plot.igraph(i.Bubbs.Outlet.Vidette.below.2011)
a.Bubbs.Outlet.Vidette.below.2011<-as_adjacency_matrix(i.Bubbs.Outlet.Vidette.below.2011,sparse=T)

i.Bubbs.Outlet.Vidette.below.2012<-ToIgraph(Bubbs.Outlet.Vidette.below.2012)
plot.igraph(i.Bubbs.Outlet.Vidette.below.2012)
a.Bubbs.Outlet.Vidette.below.2012<-as_adjacency_matrix(i.Bubbs.Outlet.Vidette.below.2012,sparse=T)


#KERN
i.KERN.10029<-ToIgraph(KERN.10029)
plot.igraph(i.KERN.10029)
a.KERN.10029<-as_adjacency_matrix(i.KERN.10029,sparse=T)

i.KERN.10030<-ToIgraph(KERN.10030)
plot.igraph(i.KERN.10030)
a.KERN.10030<-as_adjacency_matrix(i.KERN.10030,sparse=T)

i.KERN.10031<-ToIgraph(KERN.10031)
plot.igraph(i.KERN.10031)
a.KERN.10031<-as_adjacency_matrix(i.KERN.10031,sparse=T)

i.KERN.10032<-ToIgraph(KERN.10032)
plot.igraph(i.KERN.10032)
a.KERN.10032<-as_adjacency_matrix(i.KERN.10032,sparse=T)

i.KERN.10033<-ToIgraph(KERN.10033)
plot.igraph(i.KERN.10033)
a.KERN.10033<-as_adjacency_matrix(i.KERN.10033,sparse=T)

i.KERN.10034<-ToIgraph(KERN.10034)
plot.igraph(i.KERN.10034)
a.KERN.10034<-as_adjacency_matrix(i.KERN.10034,sparse=T)

i.KERN.10035<-ToIgraph(KERN.10035)
plot.igraph(i.KERN.10035)
a.KERN.10035<-as_adjacency_matrix(i.KERN.10035,sparse=T)

i.KERN.10036<-ToIgraph(KERN.10036)
plot.igraph(i.KERN.10036)
a.KERN.10036<-as_adjacency_matrix(i.KERN.10036,sparse=T)

i.KERN.10037<-ToIgraph(KERN.10037)
plot.igraph(i.KERN.10037)
a.KERN.10037<-as_adjacency_matrix(i.KERN.10037,sparse=T)

i.KERN.10038<-ToIgraph(KERN.10038)
plot.igraph(i.KERN.10038)
a.KERN.10038<-as_adjacency_matrix(i.KERN.10038,sparse=T)

i.KERN.10039<-ToIgraph(KERN.10039)
plot.igraph(i.KERN.10039)
a.KERN.10039<-as_adjacency_matrix(i.KERN.10039,sparse=T)

i.KERN.10040<-ToIgraph(KERN.10040)
plot.igraph(i.KERN.10040)
a.KERN.10040<-as_adjacency_matrix(i.KERN.10040,sparse=T)

i.KERN.10041<-ToIgraph(KERN.10041)
plot.igraph(i.KERN.10041)
a.KERN.10041<-as_adjacency_matrix(i.KERN.10041,sparse=T)

i.KERN.10042<-ToIgraph(KERN.10042)
plot.igraph(i.KERN.10042)
a.KERN.10042<-as_adjacency_matrix(i.KERN.10042,sparse=T)

i.KERN.10044<-ToIgraph(KERN.10044)
plot.igraph(i.KERN.10044)
a.KERN.10044<-as_adjacency_matrix(i.KERN.10044,sparse=T)

i.KERN.10046<-ToIgraph(KERN.10046)
plot.igraph(i.KERN.10046)
a.KERN.10046<-as_adjacency_matrix(i.KERN.10046,sparse=T)

i.KERN.10047<-ToIgraph(KERN.10047)
plot.igraph(i.KERN.10047)
a.KERN.10047<-as_adjacency_matrix(i.KERN.10047,sparse=T)

i.KERN.10048<-ToIgraph(KERN.10048)
plot.igraph(i.KERN.10048)
a.KERN.10048<-as_adjacency_matrix(i.KERN.10048,sparse=T)

i.KERN.10049<-ToIgraph(KERN.10049)
plot.igraph(i.KERN.10049)
a.KERN.10049<-as_adjacency_matrix(i.KERN.10049,sparse=T)

i.KERN.10052<-ToIgraph(KERN.10052)
plot.igraph(i.KERN.10052)
a.KERN.10052<-as_adjacency_matrix(i.KERN.10052,sparse=T)

i.KERN.10053<-ToIgraph(KERN.10053)
plot.igraph(i.KERN.10053)
a.KERN.10053<-as_adjacency_matrix(i.KERN.10053,sparse=T)

i.KERN.10054<-ToIgraph(KERN.10054)
plot.igraph(i.KERN.10054)
a.KERN.10054<-as_adjacency_matrix(i.KERN.10054,sparse=T)

i.KERN.10055<-ToIgraph(KERN.10055)
plot.igraph(i.KERN.10055)
a.KERN.10055<-as_adjacency_matrix(i.KERN.10055,sparse=T)

i.KERN.10056<-ToIgraph(KERN.10056)
plot.igraph(i.KERN.10056)
a.KERN.10056<-as_adjacency_matrix(i.KERN.10056,sparse=T)



#Calcualte betas

N<-list(i.KERN.10029,i.KERN.10030,i.KERN.10031,i.KERN.10032,i.KERN.10033,i.KERN.10034,i.KERN.10035,i.KERN.10036,i.KERN.10037,i.KERN.10038,i.KERN.10039,i.KERN.10040,i.KERN.10041,i.KERN.10042,i.KERN.10044,i.KERN.10046,i.KERN.10047,i.KERN.10048,i.KERN.10049,i.KERN.10052,i.KERN.10053,i.KERN.10054,i.KERN.10055,i.KERN.10056,
        i.Cascade.CLS1_1,i.Cascade.CLS1_3,i.Cascade.CLS2_1,i.Cascade.CLS2_3,i.Cascade.CLS3_1,i.Cascade.CLS3_2,i.Cascade.CLS3_3,i.Cascade.CLS4_1,i.Cascade.CLS4_2,i.Cascade.CLS4_3,i.Cascade.CLS5_1,
        i.EVO.ELS2_1,i.EVO.ELS2_2,i.EVO.ELS2_3,i.EVO.ELS3_1,i.EVO.ELS3_2,i.EVO.ELS3_3,i.EVO.ELS4_1,i.EVO.ELS4_2,i.EVO.ELS4_3,i.EVO.ELS5_1,i.EVO.ELS5_2,i.EVO.ELS5_3,i.EVO.ELS6_1,i.EVO.ELS6_2,i.EVO.ELS6_3,i.EVO.ELS7_1,i.EVO.ELS7_2,i.EVO.ELS7_3,i.EVO.ELS8_1,i.EVO.ELS8_2,i.EVO.ELS8_3,
        i.BUBBS.Outlet.10477.trt.2003,i.BUBBS.Outlet.10477.trt.2004,i.BUBBS.Outlet.10477.trt.2011,i.BUBBS.Outlet.10487.trt.2003,i.BUBBS.Outlet.10487.trt.2004,i.BUBBS.Outlet.10487.trt.2011,i.Bubbs.Outlet.10494.trt.2012,i.BUBBS.Outlet.11007.fishless.2003,i.BUBBS.Outlet.11007.fishless.2004,i.BUBBS.Outlet.11007.fishless.2011,i.Bubbs.Outlet.Vidette.below.2003,i.Bubbs.Outlet.Vidette.below.2004,i.Bubbs.Outlet.Vidette.below.2011,i.Bubbs.Outlet.Vidette.below.2012,
        i.RAE.RLS1_3,i.RAE.RLS2_1,i.RAE.RLS3_1,i.RAE.RLS3_2,i.RAE.RLS4_1,i.RAE.RLS4_2,i.RAE.RLS4_3,i.RAE.RLS5_1,i.RAE.RLS5_2,i.RAE.RLS5_3,i.RAE.RLS6_1,i.RAE.RLS6_2)

names(N)<-c("10029",	"10030"	,"10031"	,"10032",	"10033",	"10034"	,"10035"	,"10036"	,"10037",	"10038",	"10039"	,"10040"	,"10041",	"10042",	"10044",	"10046"	,"10047"	,"10048",	"10049"	,"10052",	"10053",	"10054",	"10055",	"10056",
            "CLS1_1",	"CLS1_3",	"CLS2_1",	"CLS2_3",	"CLS3_1",	"CLS3_2",	"CLS3_3",	"CLS4_1",	"CLS4_2",	"CLS4_3",	"CLS5_1",
            "ELS2_1","ELS2_2","ELS2_3","ELS3_1","ELS3_2","ELS3_3","ELS4_1","ELS4_2","ELS4_3","ELS5_1","ELS5_2","ELS5_3","ELS6_1","ELS6_2","ELS6_3","ELS7_1","ELS7_2","ELS7_3","ELS8_1","ELS8_2","ELS8_3",
            "Outlet.10477.trt.2003",	"Outlet.10477.trt.2004",	"Outlet.10477.trt.2011",	"Outlet.10487.trt.2003",	"Outlet.10487.trt.2004",	"Outlet.10487.trt.2011",	"Outlet.10494.trt.2012",	"Outlet.11007.fishless.2003",	"Outlet.11007.fishless.2004",	"Outlet.11007.fishless.2011",	"Outlet.Vidette.below.2003",	"Outlet.Vidette.below.2004",	"Outlet.Vidette.below.2011",	"Outlet.Vidette.below.2012",
            "RLS1_3","RLS2_1",	"RLS3_1",	"RLS3_2",	"RLS4_1",	"RLS4_2",	"RLS4_3",	"RLS5_1",	"RLS5_2",	"RLS5_3",	"RLS6_1",	"RLS6_2")
            

N.kern<-list(i.KERN.10029,i.KERN.10030,i.KERN.10031,i.KERN.10032,i.KERN.10033,i.KERN.10034,i.KERN.10035,i.KERN.10036,i.KERN.10037,i.KERN.10038,i.KERN.10039,i.KERN.10040,i.KERN.10041,i.KERN.10042,i.KERN.10044,i.KERN.10046,i.KERN.10047,i.KERN.10048,i.KERN.10049,i.KERN.10052,i.KERN.10053,i.KERN.10054,i.KERN.10055,i.KERN.10056)
names(N.kern)<-c("10029",	"10030"	,"10031"	,"10032",	"10033",	"10034"	,"10035"	,"10036"	,"10037",	"10038",	"10039"	,"10040"	,"10041",	"10042",	"10044",	"10046"	,"10047"	,"10048",	"10049"	,"10052",	"10053",	"10054",	"10055",	"10056")
                    
N.casc<-list(i.Cascade.CLS1_1,i.Cascade.CLS1_3,i.Cascade.CLS2_1,i.Cascade.CLS2_3,i.Cascade.CLS3_1,i.Cascade.CLS3_2,i.Cascade.CLS3_3,i.Cascade.CLS4_1,i.Cascade.CLS4_2,i.Cascade.CLS4_3,i.Cascade.CLS5_1)
names(N.casc)<-c("CLS1_1",	"CLS1_3",	"CLS2_1",	"CLS2_3",	"CLS3_1",	"CLS3_2",	"CLS3_3",	"CLS4_1",	"CLS4_2",	"CLS4_3",	"CLS5_1")

N.evo<-list(i.EVO.ELS2_1,i.EVO.ELS2_2,i.EVO.ELS2_3,i.EVO.ELS3_1,i.EVO.ELS3_2,i.EVO.ELS3_3,i.EVO.ELS4_1,i.EVO.ELS4_2,i.EVO.ELS4_3,i.EVO.ELS5_1,i.EVO.ELS5_2,i.EVO.ELS5_3,i.EVO.ELS6_1,i.EVO.ELS6_2,i.EVO.ELS6_3,i.EVO.ELS7_1,i.EVO.ELS7_2,i.EVO.ELS7_3,i.EVO.ELS8_1,i.EVO.ELS8_2,i.EVO.ELS8_3)
names(N.evo)<-c("ELS2_1","ELS2_2","ELS2_3","ELS3_1","ELS3_2","ELS3_3","ELS4_1","ELS4_2","ELS4_3","ELS5_1","ELS5_2","ELS5_3","ELS6_1","ELS6_2","ELS6_3","ELS7_1","ELS7_2","ELS7_3","ELS8_1","ELS8_2","ELS8_3")

N.bubbs<-list(i.BUBBS.Outlet.10477.trt.2003,i.BUBBS.Outlet.10477.trt.2004,i.BUBBS.Outlet.10477.trt.2011,i.BUBBS.Outlet.10487.trt.2003,i.BUBBS.Outlet.10487.trt.2004,i.BUBBS.Outlet.10487.trt.2011,i.Bubbs.Outlet.10494.trt.2012,i.BUBBS.Outlet.11007.fishless.2003,i.BUBBS.Outlet.11007.fishless.2004,i.BUBBS.Outlet.11007.fishless.2011,i.Bubbs.Outlet.Vidette.below.2003,i.Bubbs.Outlet.Vidette.below.2004,i.Bubbs.Outlet.Vidette.below.2011,i.Bubbs.Outlet.Vidette.below.2012)
names(N.bubbs)<-c( "Outlet.10477.trt.2003",	"Outlet.10477.trt.2004",	"Outlet.10477.trt.2011",	"Outlet.10487.trt.2003",	"Outlet.10487.trt.2004",	"Outlet.10487.trt.2011",	"Outlet.10494.trt.2012",	"Outlet.11007.fishless.2003",	"Outlet.11007.fishless.2004",	"Outlet.11007.fishless.2011",	"Outlet.Vidette.below.2003",	"Outlet.Vidette.below.2004",	"Outlet.Vidette.below.2011",	"Outlet.Vidette.below.2012")

N.rae<-list(i.RAE.RLS1_3,i.RAE.RLS2_1,i.RAE.RLS3_1,i.RAE.RLS3_2,i.RAE.RLS4_1,i.RAE.RLS4_2,i.RAE.RLS4_3,i.RAE.RLS5_1,i.RAE.RLS5_2,i.RAE.RLS5_3,i.RAE.RLS6_1,i.RAE.RLS6_2)
names(N.rae)<-c("RLS1_3","RLS2_1",	"RLS3_1",	"RLS3_2",	"RLS4_1",	"RLS4_2",	"RLS4_3",	"RLS5_1",	"RLS5_2",	"RLS5_3",	"RLS6_1",	"RLS6_2")


#Calc beta each ent
kern.beta <- network_betadiversity(N.kern)
casc.beta <- network_betadiversity(N.casc)
evo.beta <- network_betadiversity(N.evo)
bubbs.beta <- network_betadiversity(N.bubbs)
rae.beta <- network_betadiversity(N.rae)


#Geo.dist all sites
evoenv<-env%>%dplyr::filter(O.NET=="KERN")%>%dplyr::select(Site,Lat,Lon)%>%column_to_rownames(var="Site")
GeoDist.kern <- spDists(as.matrix(evoenv, latlon=TRUE))
GeoDist.kern <- as.dist(GeoDist.kern)

evoenv<-env%>%dplyr::filter(Network=="CASCADE")%>%dplyr::select(Site,Lat,Lon)%>%column_to_rownames(var="Site")
GeoDist.casc <- spDists(as.matrix(evoenv, latlon=TRUE))
GeoDist.casc <- as.dist(GeoDist.casc)

evoenv<-env%>%dplyr::filter(Network=="EVO")%>%dplyr::select(Site,Lat,Lon)%>%column_to_rownames(var="Site")
GeoDist.evo <- spDists(as.matrix(evoenv, latlon=TRUE))
GeoDist.evo <- as.dist(GeoDist.evo)

evoenv<-env%>%dplyr::filter(Network=="BUBBS")%>%dplyr::select(Site,Lat,Lon)%>%column_to_rownames(var="Site")
GeoDist.bubbs <- spDists(as.matrix(evoenv, latlon=TRUE))
GeoDist.bubbs <- as.dist(GeoDist.bubbs)

evoenv<-env%>%dplyr::filter(Network=="RAE")%>%dplyr::select(Site,Lat,Lon)%>%column_to_rownames(var="Site")
GeoDist.rae <- spDists(as.matrix(evoenv, latlon=TRUE))
GeoDist.rae <- as.dist(GeoDist.rae)


###Put it togerther

kern.beta$GEO<-GeoDist.kern
casc.beta$GEO<-GeoDist.casc
evo.beta$GEO<-GeoDist.evo
bubbs.beta$GEO<-GeoDist.bubbs
rae.beta$GEO<-GeoDist.rae

kern.beta$Network<-c("KERN")
casc.beta$Network<-c("CASCADE")
evo.beta$Network<-c("EVO")
bubbs.beta$Network<-c("BUBBS")
rae.beta$Network<-c("RAE")


alls<-rbind(kern.beta,casc.beta,evo.beta,bubbs.beta,rae.beta)
alls<-alls%>%mutate(ST.WN=ST/WN)

alls%>%ggplot( aes(x=ST,y=S,colour=Network))+
  geom_point()+geom_smooth(method = "lm")+
  geom_abline(intercept = 0, slope = 1, color="black",linetype="dashed", size=1.5)+ylim(0,1)+xlim(0,1)+
  facet_grid(~Network)+
  theme_bw()

#Dissim in species comp
alls%>%ggplot( aes(x=GEO,y=S,colour=Network))+
  geom_point()+geom_smooth(method = "lm")+
  facet_grid(~Network,scales = "free")+
  theme_bw()
#Dissimilarity of interactions due to species turnover
alls%>%ggplot( aes(x=GEO,y=ST,colour=Network))+
  geom_point()+geom_smooth(method = "lm")+
  facet_grid(~Network,scales = "free")+
  theme_bw()

#Dissimialirty of all interctions
alls%>%ggplot( aes(x=GEO,y=WN,colour=Network))+
  geom_point()+geom_smooth(method = "lm")+
  facet_grid(~Network,scales = "free")+
  theme_bw()


#Dissimilarity of interactions established between species common to both realisations

alls%>%ggplot( aes(x=GEO,y=OS,colour=Network))+
  geom_point()+geom_smooth(method = "lm")+
  facet_grid(~Network,scales = "free")+
  theme_bw()

#Contribution of species dissimilarity to network dissimilarity

alls%>%ggplot( aes(x=GEO,y=ST.WN,colour=Network))+
  geom_point()+geom_smooth(method = "lm")+
  facet_grid(~Network,scales = "free")+
  theme_bw()





evoenv<-env%>%dplyr::filter(Network!="YOUNG")%>%dplyr::select(Site,Elevation)%>%column_to_rownames(var="Site")

net.dist <- dist(as.matrix(evoenv, latlon=TRUE))
#colnames(net.dist) <- rownames(GeoDist) <- rownames(evoenv)
GeoDist <- as.dist(net.dist)


evoenv<-env%>%dplyr::filter(Network!="YOUNG")%>%dplyr::select(Site,River.dist.lake)%>%column_to_rownames(var="Site")

river.dist <- dist(as.matrix(evoenv))
colnames(river.dist) <- rownames(GeoDist) <- rownames(evoenv)
GeoDist <- as.dist(net.dist)

evoenv<-env%>%dplyr::filter(Network!="YOUNG")%>%dplyr::select(Site,SHRUB_SCRUB,Up.Lake.area,Head.river.dist,River.dist.lake,Elevation)%>%column_to_rownames(var="Site")

env.dist<-dist(evoenv)
metaweb<-metaweb(N)
net.beta <- network_betadiversity(N)
net.beta$GEO<-GeoDist
net.beta$netdist<-net.dist
net.beta$env<-env.dist
net.beta$riverdist<-river.dist


evoenv<-env%>%dplyr::filter(Network!="YOUNG")
net.beta$Network<-evo$env$Network

ggplot(net.beta, aes(x=ST,y=S))+geom_point()+geom_smooth(method = "lm")+geom_abline(intercept = 0, slope = 1, color="red", 
                                                                                    linetype="dashed", size=1.5)+ylim(0,1)+xlim(0,1)

ggplot(net.beta, aes(x=GEO,y=S))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=GEO,y=OS))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=GEO,y=WN))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=GEO,y=ST))+geom_point()+geom_smooth(method = "lm")


ggplot(net.beta, aes(x=netdist,y=S))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=netdist,y=OS))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=netdist,y=WN))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=netdist,y=ST))+geom_point()+geom_smooth(method = "lm")

ggplot(net.beta, aes(x=env,y=S))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=env,y=OS))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=env,y=WN))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=env,y=ST))+geom_point()+geom_smooth(method = "lm")

ggplot(net.beta, aes(x=river.dist,y=S))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=river.dist,y=OS))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=river.dist,y=WN))+geom_point()+geom_smooth(method = "lm")
ggplot(net.beta, aes(x=river.dist,y=ST))+geom_point()+geom_smooth(method = "lm")


