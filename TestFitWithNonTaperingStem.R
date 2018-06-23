library(TapeR)
data("DxHx.df")
DxHx.df.bad <- DxHx.df[DxHx.df$Hx/DxHx.df$Ht <.9, ]
DxHx.df.bad$Dx[DxHx.df.bad$Id == 2201 & DxHx.df.bad$Hx == 7] <- 27

DxHx.df.bad %>% group_by(Id) %>% mutate(tpr = Dx - lead(Dx)) %>% filter(tpr<=0)

#prepare the data (could be defined in the function directly)
Id = DxHx.df.bad[,"Id"]
x = DxHx.df.bad[,"Hx"]/DxHx.df.bad[,"Ht"]#calculate relative heights
y = DxHx.df.bad[,"Dx"]

#define the relative knot positions and order of splines
knt_x = c(0.0, 0.1, 0.75, 1.0);	ord_x = 4 # B-Spline knots: fix effects; order (cubic = 4)
knt_z = c(0.0, 0.1       ,1.0); ord_z = 4 # B-Spline knots: rnd effects

#fit the model
#debugonce(TapeR_FIT_LME.f)
taper.model <- TapeR_FIT_LME.f(Id, x, y, knt_x, ord_x, knt_z, ord_z,
                               IdKOVb = "pdSymm")
