# SLIDE
First install `SLIDE` in Python by running the following code from a  R command terminal:



```library(devtools)```   
```install_github("jishnu-lab/SLIDE")```


To run the slide function, use the following command:

```library(doParallel)```

For capturing the interaction effect ```do_interacts = TRUE```


```res <- SLIDE(z,y,method = 4,do_interacts = TRUE)```


   
