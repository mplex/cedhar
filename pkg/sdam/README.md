# sdam

## Digital tools for the SDAM project at Aarhus University

This package provides digital tools for the [*Social Dynamics and complexity
in the Ancient Mediterranean*](https://sdam-au.github.io/sdam-au/) (SDAM) project 
at the School of Culture and Society at Aarhus University.



<br/>

### Installation

You can install the `sdam` package from these GitHub repositories using the R console or RStudio if you wish.

```r
## install beta version
devtools::install_github("mplex/cedhar", subdir="pkg/sdam")
```
or 

```r
## install release candidate
devtools::install_github("sdam-au/sdam")
```


### Usage

```r
## Load package
library("sdam")
packageVersion("sdam")
#[1] �0.4.5�
```

```r
## Load epigraphic data
data("EDH")
```

```r
# Look at how many inscriptions?
length(EDH)
# [1] 84701
```


```r
# Also look at the object structure
str(EDH)
```


### Get particular inscriptions

For instance, the following commands extract people named in Roman inscriptions in Iudaea and record them 
in object `iud`.


```r
# obtain epigraphic data from Iudaea
iud <- get.edh(search="inscriptions", province="Iud")
```

```r
# how many inscriptions in Iudaea?
length(iud)
# [1] 187
```


### Relative dating

Since `sdam` v0.2.0 there is the option in `edhw()` to extract variables from a `EDH` dataset *fragments*. 
To place Roman inscriptions in Iudaea into a data frame object `iud_df`

```r
# extract variables of interes from object `iud` and convert it into a data frame
iud_df <- edhw(vars=c("not_after", "not_before"), x=iud, as="df", na.rm=FALSE)

# what object types is?
is(iud_df)
#[1] "data.frame" "list"       "oldClass"   "vector"  
```


To remove missing data form `iud_df`, we activate `na.rm` argument in the function as for `iud_df0`

```r
# extract variables of interes from object `iud` and convert it into a data frame
iud_df0 <- edhw(vars=c("not_after", "not_before"), x=iud, as="df", na.rm=TRUE)
```

Now we compare outcomes with these two options.

```r
cbind(head(iud_df,10), head(iud_df0,10))
#         id not_before not_after       id not_before not_after
#1  HD001461       0071      0130 HD001461       0071      0130
#2  HD001958       0301      0400 HD001958       0301      0400
#3  HD001964       0132      0135 HD001964       0132      0135
#4  HD001973       0071      0130 HD001973       0071      0130
#5  HD001985       0198      0209 HD001985       0198      0209
#6  HD002376       0051      0220 HD002376       0051      0220
#7  HD004074       0026      0036 HD004074       0026      0036
#8  HD004735       0070      0079 HD004735       0070      0079
#9  HD006228       0212      0220 HD006228       0212      0220
#10 HD007068       <NA>      <NA> HD011646       0117      0138
```

Hence, not available data is removed in `iud_df0`.



### People

It is possible to extract the `people` component from the `EDH` API dataset, which is an integrated list with their own items.

For instance, the following commands extract people named in Roman inscriptions in Iudaea and Syria into a data frame object 
with a *long* (default) and a data frame with a *wide* format:

```r
edhw(x=get.edh(search="inscriptions", province="Iud"), vars="people", as="df")
edhw(x=get.edh(search="inscriptions", province="Syr"), vars="people", as="df", wide=TRUE)
```

The same can apply to the rest of Roman provinces recorded in the `EDH` dataset.


#### Specifying output

It is also possible to `limit` the amount of records to process, and with the following example we can extract the first four 
inscriptions by `hd_nr` id in a data frame with a long format.

```r
edhw(vars="people", as="df", limit=4)
#        id age: years             cognomen gender                                              name    nomen person_id praenomen           status   tribus
#1 HD000001       <NA>               Optata female                               Noniae P.f. Optatae    Nonia         1      <NA>             <NA>     <NA>
#2 HD000001       <NA>               Artemo   male                                 C. Iulio Artemoni   Iulius         2        C.             <NA>     <NA>
#3 HD000001       <NA>              Optatus   male                            C. Iulius C.f. Optatus   Iulius         3        C.             <NA>     <NA>
#4 HD000002         70                Paris   male                                  C. Sextius Paris  Sextius         1        C.             <NA>     <NA>
#5 HD000003       <NA> Sisenna+ Rutilianus+   male      [P. M]ummio [P.f. Gal. S]isenna[e Rutiliano] Mummius+         1       P.+ senatorial order Galeria+
#6 HD000004       <NA>                [---]   <NA> [---?]AV(?)S(?)[---]L(?)L(?)A M. Porci Nigri ser.     <NA>         1      <NA>           slaves     <NA>
#7 HD000004       <NA>                Niger   <NA>                                    M. Porci Nigri  Porcius         2        M.             <NA>     <NA>
```

while a specific record is retrieved by using the `id` argument as follows:


```r
edhw(vars="people", as="df", id=4444)
#        id     cognomen gender                   name    nomen person_id praenomen           status
#1 HD004444    Gordianus   male     M. Antoni Gordiani Antonius         1        M.             <NA>
#2 HD004444 Tranquillina female Sabiniae Tranquillinae  Sabinia         2      <NA>             <NA>
#3 HD004444  Mercurialis   male       Iul. Mercurialis  Iulius*         3      <NA> equestrian order
```


### Probability of existence

Since `sdam` v0.3.0 there is a dedicated function to compute probability of existence of date points or intervals. 
We obtain inscriptions from Iudaea in EDH data base, and then compute the aoristic sum with a 5-bin chronological phases. 

```r
prex(x=get.edh(search="inscriptions", province="Iud"), vars=c("not_before", "not_after"), cp="bin5")
#   Arch     Class      Hell       Rom       Byz 
#    0.000     0.000  1337.904 13405.017     0.000
```

Since most of the inscriptions are within the Roman period, we try an eight-bin chronological phases option. 

```r
prex(x=get.edh(search="inscriptions", province="Iud"), vars=c("not_before", "not_after"), cp="bin8")
#   Arch     Class      Hell      ERom      MRom      LRom      EByz      LByz 
#   0.0000    0.0000 1337.9040 2396.4529 1200.5623  320.5379    0.0000    0.0000
```




### Vignettes

* [Plotting ancient Roman provinces and regions](https://sdam-au.github.io/sdam/articles/RomanProvs.html)
* [Re-encoding Greek characters from 'EDH' dataset](https://sdam-au.github.io/sdam/articles/Encoding.html)


<!-- Look at the ['sdam' R package documentation](https://sdam-au.github.io/sdam/) to find more about the distinct variables in `EDH` dataset 
and applications with the different functions. -->


### See also
* [`"sdam"` manual](https://github.com/mplex/cedhar/blob/master/typesetting/reports/sdam.pdf)
* [Social Dynamics and complexity in the Ancient Mediterranean Project](https://sdam-au.github.io/sdam-au/)

