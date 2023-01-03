df <- mtcars
df$cyl <- as.factor(df$cyl)
df$gear <- as.factor(df$gear)
df$carb <- as.factor(df$carb)

f_contY_contX <- formula(mpg ~ disp + hp)
f_contY_mixedX <- formula(mpg ~ disp + cyl)
f_contY_catX <- formula(mpg ~ cyl + gear)
f_contY_contX.contX <- formula(mpg ~ disp + hp + disp:hp)
f_contY_contX.catX <- formula(mpg ~ disp + cyl + disp:cyl)
f_contY_catX.catX <- formula(mpg ~ cyl + gear + cyl:gear)
f_contY_all <- formula(mpg ~ disp + hp + cyl + gear + carb + disp:cyl + cyl:gear)
