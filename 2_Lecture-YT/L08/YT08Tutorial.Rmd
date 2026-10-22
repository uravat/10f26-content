---
title: "Your Turn : Simulating Random variables in `R`"
author: "Dr. Uma Ravat"
date: "`r Sys.Date()`"
tutorial:
  id: "Lecture08_Random Variables"
output: 
  learnr::tutorial:
    progressive: true
    allow_skip: true
description: "Simulating Random variables in `R"
runtime: shiny_prerendered
---


```{r setup, include = FALSE}
library(learnr)
library(tutorial.helpers)
library(tidyverse)
knitr::opts_chunk$set(echo = FALSE)
options(tutorial.exercise.timelimit = 60, 
        tutorial.storage = "local") 
```

```{r, echo = F}
bfcolor <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{\\textbf{%s}}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'><b>%s</b></span>", color, x)
  } else x
}
```


```{r copy-code-chunk, child = system.file("child_documents/copy_button.Rmd", package = "tutorial.helpers")}
```

```{r info-section, child = system.file("child_documents/info_section.Rmd", package = "tutorial.helpers")}
```


## Random variables, expected value, variance
### A card game 

In a game of cards you win \$1 if you draw a heart, \$5 if you draw an ace (including the ace of hearts), \$10 if you draw the king of spades and nothing for any other card you draw. 

### Exercise : Theory

Using R, tabluate the pmf of your winnings. Verify that it is a pmf. What is the expected value of your winnings? Interpret what this value means. 

```{r game-ev, exercise=TRUE, exercise.eval=FALSE}
your_code_here
```

```{r game-ev-solution}
values <- c(0, 1, 5, 10)
p <- c(35/52, 12/52, 4/52, 1/52)
names(p) <- values
p
round(p,2)

sum(p)

ex <- sum(values*p)
ex
```

### Exercise: Simulation

Simulate playing this game 100 times. ie draw a sample of size 100 from the probability distribution of winnings from this game. View the results appropriately and find the average of the winnings in these 100 games. 

_Hint:_

- `sample()`: what values and with what probability, what's the sample size?
- What type of plot? (hint: Data falls into categories)


```{r game-sim100, exercise=TRUE, exercise.eval=FALSE}
your_code_here
```

```{r game-sim100-solution}
values <- c(0,1,5,10)
p <- c(35/52, 12/52, 4/52, 1/52 )
names(p) <- values

sim_wins <- sample( values, 100, replace = TRUE, prob = p)
sim_wins

round(p,2)

table(sim_wins)

prop.table(table(sim_wins))

print(paste("average of winnings in 100 simulated games is", round(mean(sim_wins),2)))

print(paste("expect value of winnings  is", round(sum(values*p),2)))

barplot(table(sim_wins))
```

### Exercise

Compare the average winnings from simulation and theory. Are they same, different, how do you explain it?

B

### Exercise

What is the variability in winnings? What is its interpretation?

```{r game-var, exercise=TRUE, exercise.eval=FALSE}
your_code_here
```

```{r game-var-solution}
values <- c(0, 1, 5, 10)
p <- c(35/52, 12/52, 4/52, 1/52 )
names(p) <- values

ev <- sum(values*p)
ev

v <-  sum(((values-ev)^2)*p)
v

sd <- sqrt(v)
sd

ev - sd

ev + sd

```

### Exercise

Find the cdf at 7. 

```{r game-cdf, exercise=TRUE, exercise.eval=FALSE}
your_code_here (see solution if you need to!)
```

```{r game-cdf-solution}
values <- c(0,1,5,10)
p <- c(35/52, 12/52, 4/52, 1/52 )
names(p) <- values
p

# Calculate CDF
cp <- cumsum(p)
cp



x_vals = as.numeric(names(cp))


# Create step function plot
plot(x_vals, cp, 
     type = "s",
     main = "CDF of Winnings from Card Game",
     xlab = "Winnings (x) in dollars",
     ylab = "P(X ≤ x)",
     col = "darkgreen",
     lwd = 2,
     xlim = c(-1, 11),
     ylim = c(0, 1))

# Add closed circles at actual values (value IS included)
points(x_vals, cp, pch = 16, col = "darkgreen", cex = 1.5)

# Add open circles at jump points (value NOT included just before)
points(x_vals[-1], cp[-length(cp)], pch = 1, col = "darkgreen", cex = 1.5, lwd = 2)

# Add grid
grid()

# Add legend
legend("bottomright", 
       legend = c("Value included (≤)", "Value not included (<)"),
       pch = c(16, 1), 
       col = "darkgreen",
       bty = "n")

# Add annotations for interesting points
text(5, cp["5"] - 0.05, paste("P(X ≤ 5) =", round(cp["5"], 3)), pos = 3, cex = 0.8)


# CDF at 7 is cp at 3!
cp[3]

```

## Clinical Trial Simulation: pmf from simulation

### Introduction: What Are We Simulating?

**Scenario**: A pharmaceutical company is testing a new drug on 8 patients. Based on preliminary studies, they believe each patient has a 15% chance of responding well to the drug.

**Our Goal**: Instead of running 500 actual clinical trials (which would be expensive and time-consuming!), we'll use R to simulate what might happen if we ran 500 trials. This helps us understand the distribution of possible outcomes.

---

### Step 1: Understanding ONE Trial First

Before we simulate 500 trials, let's understand what happens in a SINGLE trial with 8 patients.

**Conceptual Question**: If you flip a coin 8 times, you might get different numbers of heads each time you do this experiment. Similarly, in one trial with 8 patients, we might see 0, 1, 2, ... or even all 8 patients respond well. How do we simulate this?

```{r trial-step1, exercise=TRUE, exercise.eval=FALSE}
# STEP 1: Set up the basics for ONE trial

# How many patients in one trial?
number.patients = 8

# What's the probability each patient responds well?
response.prob = 0.15

# Let's simulate ONE trial: 8 patients, each has 15% chance of responding (1) or not (0)
set.seed(123)  # So we all get the same "random" results
one_trial = sample(c(0, 1),                    # 0 = no response, 1 = good response
                   size = number.patients,      # 8 patients
                   prob = c(0.85, 0.15),       # 85% chance of 0, 15% chance of 1
                   replace = TRUE)              # Each patient is independent

# Look at the results for each patient
print("Results for each of the 8 patients (0=no response, 1=good response):")
print(one_trial)

# How many patients responded well in THIS trial?
print(paste("Total number of good responses in this trial:", sum(one_trial)))
```

**Questions to think about**:

- Why do we use `replace = TRUE`?
- What does `prob = c(0.85, 0.15)` mean?
- Why is the sum of the vector equal to the number of good responses?

---

### Step 2: Running Multiple Trials (Using the Loop Concept)

Now that we understand ONE trial, let's run a few trials and see how results vary.

```{r trial-step2, exercise=TRUE, exercise.eval=FALSE}
# STEP 2: Run 5 trials to see how results vary

number.patients = 8
response.prob = 0.15

set.seed(456)

# We'll store the number of good responses from each trial
results_from_5_trials = c()  # Empty vector to fill

# Run 5 trials
for(trial_number in 1:5) {
  
  # Simulate one trial
  one_trial = sample(c(0, 1), 
                     size = number.patients,
                     prob = c(1 - response.prob, response.prob),
                     replace = TRUE)
  # Look at the results for each patient
  print("Results for each of the 8 patients (0=no response, 1=good response):")
  print(one_trial)
  
  # Count how many patients responded well
  good_responses = sum(one_trial)
  
  
 # How many patients responded well in THIS trial?
  print(paste("Total number of good responses in this trial:", sum(one_trial)))
  print( " ** "        )
  
  
  
  # Store this result
  results_from_5_trials[trial_number] = good_responses
  
  # Print what happened in this trial
  print(paste("Trial", trial_number, ": Patients who responded =", one_trial, 
              "| Total =", good_responses))
  print( " ** "        )
}

print("---Summary---")
print(paste("Results from 5 trials:", paste(results_from_5_trials, collapse = ", ")))
print(paste("Average number of good responses:", mean(results_from_5_trials)))
```

**Key Insight**: Notice how the number of good responses varies from trial to trial! Sometimes we get 0, sometimes 1, sometimes 2, etc. This is the randomness we're studying.

**Questions to think about:**(Optional) 

- Look at the `print` statements, why is it printing 8 lines rather than one for each of the trial? 

---

### Step 3: The Full Simulation (500 Trials)

Now let's scale up to 500 trials. We'll use the same logic, but we won't print each trial (that would be too much output!).

```{r trial-step3, exercise=TRUE, exercise.eval=FALSE}
# STEP 3: Run 500 trials

# Define parameters
number.patients = 8
response.prob = 0.15
number.replicates = 500

# Create empty vector to store results from all 500 trials
number.responses = vector("numeric", number.replicates)

# Set seed for reproducibility
set.seed(10202022)

# Run 500 trials
for(k in 1:number.replicates) {
  
  # Simulate one trial (8 patients)
  one_trial_results = sample(c(0, 1), 
                             size = number.patients,
                             prob = c(1 - response.prob, response.prob),
                             replace = TRUE)
  
  # Count and store the number of good responses
  number.responses[k] = sum(one_trial_results)
  
}

# Let's look at the first 20 results
print("Number of good responses in first 20 trials:")
print(number.responses[1:20])

print(paste("Simulation complete! We ran", number.replicates, "trials."))
```

**What just happened?**

- We simulated 500 different clinical trials
- Each trial had 8 patients
- Each patient had a 15% chance of responding well
- We stored how many patients responded in each trial

**Exercise:**

- Based on these results,  how many good responses would we expect to see if we really ran 500 clinical trials? 

---

### Step 4: Visualizing the Results

Numbers are great, but let's SEE what happened!

```{r trial-step4-setup}
number.patients = 8
response.prob = 0.15
number.replicates = 500
number.responses = vector("numeric", number.replicates)
set.seed(10202022)
for(k in 1:number.replicates) {
  one_trial_results = sample(c(0, 1), 
                             size = number.patients,
                             prob = c(1 - response.prob, response.prob),
                             replace = TRUE)
  number.responses[k] = sum(one_trial_results)
}
```

```{r trial-step4, exercise=TRUE, exercise.eval=FALSE, exercise.setup="trial-step4-setup"}
# STEP 4: Visualize the distribution

# Create a frequency table
freq_table = table(number.responses)
print("Frequency table (how many times did we see 0 responses, 1 response, etc.):")
print(freq_table)

# Visualize with a barplot
barplot(freq_table,
        main = "Distribution of Good Responses Across 500 Trials",
        xlab = "Number of Patients Who Responded Well",
        ylab = "Frequency (out of 500 trials)",
        col = "skyblue")

# Add some summary statistics
print(paste("Most common outcome:", names(which.max(freq_table)), "good responses"))
print(paste("Average number of good responses:", round(mean(number.responses), 2)))
```

**Interpretation Questions**:

- Which outcome happened most frequently?
- Is the distribution symmetric or skewed?
- Why do you think we see this pattern?

---

### Step 5: Building the Probability Distribution

Now we convert frequencies to probabilities (the PMF).

```{r trial-step5-setup}
number.patients = 8
response.prob = 0.15
number.replicates = 500
number.responses = vector("numeric", number.replicates)
set.seed(10202022)
for(k in 1:number.replicates) {
  one_trial_results = sample(c(0, 1), 
                             size = number.patients,
                             prob = c(1 - response.prob, response.prob),
                             replace = TRUE)
  number.responses[k] = sum(one_trial_results)
}
```

```{r trial-step5, exercise=TRUE, exercise.eval=FALSE, exercise.setup="trial-step5-setup"}
# STEP 5: Calculate probabilities (PMF)

# Convert frequencies to proportions
pmf = prop.table(table(number.responses))

print("Probability Mass Function (PMF):")
print(round(pmf, 4))

# What does this mean?
print("Interpretation:")
print(paste("P(X = 0) =", round(pmf["0"], 4), "| About", round(pmf["0"]*100, 1), "% of trials had 0 responses"))
print(paste("P(X = 1) =", round(pmf["1"], 4), "| About", round(pmf["1"]*100, 1), "% of trials had 1 response"))

# Visualize the PMF
barplot(pmf,
        main = "Estimated PMF from Simulation",
        xlab = "Number of Good Responses (X)",
        ylab = "Probability P(X = x)",
        col = "coral")
```

---

### Step 6: Expected Value and Variance

Now let's calculate the center and spread of our distribution.

```{r trial-step6-setup, echo=FALSE}
number.patients = 8
response.prob = 0.15
number.replicates = 500
number.responses = vector("numeric", number.replicates)
set.seed(10202022)
for(k in 1:number.replicates) {
  one_trial_results = sample(c(0, 1), 
                             size = number.patients,
                             prob = c(1 - response.prob, response.prob),
                             replace = TRUE)
  number.responses[k] = sum(one_trial_results)
}
```

```{r trial-step6, exercise=TRUE, exercise.eval=FALSE, exercise.setup="trial-step6-setup"}

# STEP 6: Calculate Expected Value and Variance

# Get the PMF
pmf = prop.table(table(number.responses))

# Get all possible values of X
x_values = as.numeric(names(pmf))

# Calculate E(X) = sum of [x * P(X = x)]
expected_value = sum(x_values * pmf)

print(paste("Expected Value E(X) =", round(expected_value, 3)))
print(paste("Interpretation: On average, we expect about", round(expected_value, 1), 
      "patients out of 8 to respond well"))

# Calculate Variance = sum of [(x - E(X))^2 * P(X = x)]
variance = sum((x_values - expected_value)^2 * pmf)
std_dev = sqrt(variance)

print(paste("Variance V(X) =", round(variance, 3)))
print(paste("Standard Deviation SD(X) =", round(std_dev, 3)))
print(paste("Interpretation: The number of good responses typically varies by about", 
      round(std_dev, 1), "from the expected value"))

# Compare to theoretical expectation
theoretical_ev = number.patients * response.prob
print(paste("Theoretical E(X) = n*p =", number.patients, "*", response.prob, "=", theoretical_ev))
print(paste("Difference from simulation:", round(abs(expected_value - theoretical_ev), 3)))
```

**Question to think about:**

Look at this line of code: `expected_value = sum(x_values * pmf)`

Compare it to the mathematical formula: E(X) = Σ x · P(X = x)

- What does `x_values * pmf` do?
- What does `sum()` do?
- How does the code implement the formula?

**Making the Connection:**

**Mathematical Formula:** $E(X) = sum x · P(X = x)$  
In words: "Sum of (each value times its probability)"

**R Code:** `expected_value = sum(x_values * pmf)`

Breaking it down:
- `x_values`:  the x values (0, 1, 2, 3, ...)
- `pmf`: the probabilities P(X = x)
- `x_values * pmf`:  multiply each x by its probability
- `sum()`: add them all up (this is the Σ)

**Do you see how the code is doing exactly what the formula says?**

**Question to think about:**

- Do you see how the code is doing exactly what the variance formula says?**

---

### Step 7: Cumulative Distribution Function (CDF)

The CDF tells us: "What's the probability of getting AT MOST x good responses?"

```{r trial-step7-setup}
number.patients = 8
response.prob = 0.15
number.replicates = 500
number.responses = vector("numeric", number.replicates)
set.seed(10202022)
for(k in 1:number.replicates) {
  one_trial_results = sample(c(0, 1), 
                             size = number.patients,
                             prob = c(1 - response.prob, response.prob),
                             replace = TRUE)
  number.responses[k] = sum(one_trial_results)
}
```

```{r trial-step7, exercise=TRUE, exercise.eval=FALSE, exercise.setup="trial-step7-setup"}
# STEP 7: Calculate and visualize the CDF

# Get PMF
pmf = prop.table(table(number.responses))

# Calculate CDF (cumulative sum of probabilities)
cdf = cumsum(pmf)

print("Cumulative Distribution Function (CDF):")
print(round(cdf, 4))

print("\nInterpretation:")
print(paste("P(X ≤ 0) =", round(cdf["0"], 4), "| Probability of 0 or fewer responses"))
print(paste("P(X ≤ 1) =", round(cdf["1"], 4), "| Probability of 1 or fewer responses"))
print(paste("P(X ≤ 2) =", round(cdf["2"], 4), "| Probability of 2 or fewer responses"))

# Visualize CDF as a step function with open and closed circles
x_vals = as.numeric(names(cdf))
cdf_vals = as.numeric(cdf)

# Create the step function plot
plot(x_vals, cdf_vals, 
     type = "n",  # Don't plot points yet, just set up the axes
     main = "Cumulative Distribution Function",
     xlab = "Number of Good Responses (x)",
     ylab = "P(X ≤ x)",
     xlim = c(min(x_vals) - 0.5, max(x_vals) + 0.5),
     ylim = c(0, 1))

# Add horizontal lines for each step
for(i in 1:length(x_vals)) {
  if(i == 1) {
    # First segment: from far left to first x value
    segments(x0 = min(x_vals) - 0.5, y0 = 0, 
             x1 = x_vals[i], y1 = 0, 
             col = "darkgreen", lwd = 2)
  }
  
  # Horizontal line at current CDF value
  if(i < length(x_vals)) {
    segments(x0 = x_vals[i], y0 = cdf_vals[i], 
             x1 = x_vals[i+1], y1 = cdf_vals[i], 
             col = "darkgreen", lwd = 2)
  } else {
    # Last segment extends to the right
    segments(x0 = x_vals[i], y0 = cdf_vals[i], 
             x1 = max(x_vals) + 0.5, y1 = cdf_vals[i], 
             col = "darkgreen", lwd = 2)
  }
}

# Add CLOSED circles (filled) at the actual CDF values (right side of jumps)
points(x_vals, cdf_vals, pch = 16, col = "darkgreen", cex = 1.5)

# Add OPEN circles (hollow) at the left side of jumps (showing discontinuity)
if(length(x_vals) > 1) {
  for(i in 2:length(x_vals)) {
    points(x_vals[i], cdf_vals[i-1], pch = 1, col = "darkgreen", cex = 1.5, lwd = 2)
  }
}

# Add grid for easier reading
grid()

# Add a legend explaining the circles
legend("bottomright", 
       legend = c("Closed circle: value included", "Open circle: value not included"),
       pch = c(16, 1), 
       col = "darkgreen",
       pt.cex = 1.5,
       bty = "n")

# Example: P(X ≤ 2)
print(paste("\nExample: The probability of 2 or fewer good responses is", round(cdf["2"], 4)))
```

---

### Challenge Exercises

Now that you understand the building blocks, try these:

**Exercise 1**: What if the drug is more effective (response.prob = 0.40)? Modify the code and see how the distribution changes.

```{r challenge1, exercise=TRUE, exercise.eval=FALSE}
# Modify the response probability and re-run the simulation


```

**Exercise 2**: What if we test on 20 patients instead of 8? How does this affect the distribution?

```{r challenge2, exercise=TRUE, exercise.eval=FALSE}
# Modify the number of patients and re-run


```

**Exercise 3**: Based on your simulation with original parameters (p=0.15, n=8), estimate P(X ≥ 3). That is, the probability that at least 3 patients respond well.


```{r challenge3-setup}
number.patients = 8
response.prob = 0.15
number.replicates = 500
number.responses = vector("numeric", number.replicates)
set.seed(10202022)
for(k in 1:number.replicates) {
  one_trial_results = sample(c(0, 1), 
                             size = number.patients,
                             prob = c(1 - response.prob, response.prob),
                             replace = TRUE)
  number.responses[k] = sum(one_trial_results)
}
```

```{r challenge3, exercise=TRUE, exercise.eval=FALSE, exercise.setup="challenge3-setup"}
# Hint: P(X ≥ 3) = 1 - P(X ≤ 2)


```

---

### Summary: What Did We Learn?

1. **Simulation Logic**: We can use `sample()` and loops to simulate random processes
2. **One vs. Many**: Understanding one trial helps us understand many trials
3. **Building Up**: We went from one trial → few trials → many trials
4. **Visualization**: Plots help us see patterns in the data
5. **PMF**: Describes the probability of each outcome
6. **CDF**: Describes cumulative probabilities
7. **Expected Value & Variance**: Summarize the center and spread of the distribution

**Key R Functions We Used**:

- `sample()`: Generate random samples
- `table()`: Count frequencies
- `prop.table()`: Convert counts to proportions
- `cumsum()`: Calculate cumulative sums
- `barplot()` and `plot()`: Visualize distributions


```{r download-answers, child = system.file("child_documents/download_answers.Rmd", package = "tutorial.helpers")}
```



