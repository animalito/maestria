library(mrpdata)
library(mrp)

## Load example data.
data(CCES.complete)

## Helper datasets for other US applications of MRP:
data(spmap.states) # projected US state map
data(mrp.census)   # census with common demo strata
data(mrp.regions)  # regions data.frame with DC separate

## To ensure matching of strata between poll and population,
## both should be factors with identical names and levels.
CCES.complete <- within (CCES.complete, {
    education <- factor(education,exclude=NA)
    female <- factor(sex=="Female", labels=c("Male","Female"), exclude=NA)
    race <- factor(race,exclude=NA)
    f.race <- interaction(female,race)
})

## Poll has four levels of education, so we need to combine
## the top two levels in the census data. We'll also go ahead
## and trim it down to just the variables used here.

mrp.census <- within(mrp.census,{
    age <- factor(age,exclude=NA,labels=c("18-29","30-44","45-64","65+"))
    education[education=="postgraduate"] <- "college graduate"
    education <- factor(education,exclude=NA)
    edu <- factor(education,exclude=NA,labels=c("< High School",
                                                "High School",
                                                "Some College",
                                                "Graduated College"))
    state <- factor(state,exclude=NA)
    race <- factor(race,exclude=NA)
    f.race <- interaction(sex,race)
})
mrp.census <- na.omit(mrp.census)

## Ready to run simple mrp with poll and population:
mrp.simple <- mrp(ban.gaymarr ~ state+age+education+race,
                  data=CCES.complete,
                  population=mrp.census,
                  pop.weights="weighted2004")
print(100*poststratify(mrp.simple, ~ education+age), digits=4)
print(100*poststratify(mrp.simple, ~ state+age), digits=2)
## Not run:
## Fit a fuller model, adding state-level predictors:
## This model is also used in the not-run example
## for plotting maps.
## Note: for the population.formula here, we remove 'age' to demonstrate
## the use of the arg. The more common scenario is to remove 'poll'
## when combining several polls -- each gets a varying intercept,
## but they're not included in poststratification.

mrp.statelevel <- mrp(ban.gaymarr~
                          state+f.race+age+education,
                      data=CCES.complete,
                      population=mrp.census, pop.weights="weighted2008",
                      formula.pop.update= .~.-age,
                      grouplevel.data.frames=list(Statelevel,
                                                  mrp.regions),
                      grouplevel.expressions=list(
                          expression(age.edu <- interaction(age,education)),
                          ## an ordered factor, we use a normalized
                          ## continuous z.age as the prior mean for
                          ## varying intercepts of the 'age' groups.
                          ## That is, the prior mean for
                          ## age cat 1 of 4 (18-29) becomes (-.58)
                          expression(z.age <- rescale(age)))
)
## Note: the formula is expanded from the condensed version in "formula" to
##  an expanded version.
getFormula(mrp.statelevel)

## Update the model.formula on already-prepared mrp object and re-fit:
mrp.statelevel <- mr(mrp.statelevel, .~.+(1|region)+ (1|age.edu)+
                         z.age+p.relig.full+p.kerry.full)

## Fine plot control is shown with this example in plotmrp documentation!

## End(Not run)
