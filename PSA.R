##propensity score
#define propensity score formula
covariatesNames <- c(
  "age2014", #age in 2014
  "gender2014", #gender
  "mari2014", #marriage in 2014
  "familysupport2014", #family support
  "healself2014", #SRH
  "iadl_2014", #IADL
  "cognitive2014", #cognitive function
  "eduy2014", #education
  "incoself2014", #income
  "party2014" #CCP membership 
)

ps <- glm(internetgroup2016 ~ age2014 + gender2014 + mari2014+healself2014 + iadl_2014 + 
            cognitive2014 + eduy2014+ incoself2014 + party2014+confidence2014+lifesatis2014,
          data = mergedata1416_urban_nu, family	= binomial()) 
hoslem.test(ps$y,fitted(ps))
mergedata1416_urban_nu$psvalue	<- predict(ps,	type	= "response")
mergedata1416_urban_nu$logitpsvalue<-log(mergedata1416_urban_nu$psvalue/(1-mergedata1416_urban_nu$psvalue))
psych::describe(mergedata1416_urban_nu$logitpsvalue)   ##1.44*0.2=0.288>- caliper
mergedata1416_urban_nu<-mergedata1416_urban_nu %>%
  mutate(weightsATE=ifelse(internetgroup2016==0,1/(1-psvalue),1/psvalue))
match_weight <- survey::svydesign(ids = ~1, data = mergedata1416_urban_nu, weights = mergedata1416_urban_nu$weightsATE)

library(MatchIt)
set.seed(100)
m.out = matchit(internetgroup2016 ~ age2014 + gender2014 + mari2014+healself2014 + iadl_2014 + 
                  cognitive2014 + eduy2014+ incoself2014 + party2014+confidence2014+lifesatis2014,
                data = mergedata1416_urban_nu, method = "nearest",distance="logit",ratio=5,caliper=0.2750626,m.order=3) 
summary(m.out)
mergedata1416_urban_nu.match	= match.data(m.out)

t.test(lifesatis_change~internetgroup2016,data=mergedata1416_urban_nu.match)
t.test(confidence_change~internetgroup2016,data=mergedata1416_urban_nu.match)


m.out1 = matchit(internetgroup2016 ~ age2014 + gender2014 + mari2014+healself2014 + iadl_2014 + 
                  cognitive2014 + eduy2014+ incoself2014 + party2014+confidence2014+lifesatis2014,
                data = mergedata1416_urban_nu, method = "optimal",ratio=5) 
summary(m.out1)
mergedata1416_urban_nu.match	= match.data(m.out1)

t.test(lifesatis_change~internetgroup2016,data=mergedata1416_urban_nu.match1)
t.test(confidence_change~internetgroup2016,data=mergedata1416_urban_nu.match1)

m.out_f = matchit(internetgroup2016 ~ age2014 + mari2014+healself2014 + iadl_2014 + 
                   cognitive2014 + eduy2014+ incoself2014 + party2014+confidence2014+lifesatis2014,
                 data = mergedata1416_urban_nu_female, method = "optimal",ratio=5) 
summary(m.out_f)
mergedata1416_urban_nu_female.match	= match.data(m.out_f)
t.test(lifesatis_change~internetgroup2016,data=mergedata1416_urban_nu_female.match)
t.test(confidence_change~internetgroup2016,data=mergedata1416_urban_nu_female.match)

m.out2 = matchit(internetgroup2016 ~ age2014 + gender2014 + mari2014+healself2014 + iadl_2014 + 
                  cognitive2014 + eduy2014+ incoself2014 + party2014+confidence2014+lifesatis2014,
                data = mergedata1416_urban_nu, method = "full") 
summary(m.out2)

mergedata1416_urban_nu.match2	= match.data(m.out2)

t.test(lifesatis_change~internetgroup2016,data=mergedata1416_urban_nu.match2)
t.test(confidence_change~internetgroup2016,data=mergedata1416_urban_nu.match2)

library(ggplot2)
ggplot(mergedata1416_urban_nu.match1) + aes(x=psvalue,fill=internetgroup2016)+scale_fill_grey()+
  geom_histogram()+xlab("propensity score")+ylab("Number of participants")+theme_apa()+theme(legend.title=element_blank(),legend.position = "bottom")
