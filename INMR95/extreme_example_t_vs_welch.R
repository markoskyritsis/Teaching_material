set.seed(42)
B <- 10000

# CASE 1: Equal Normal Variances (SD1 = SD2 = 1), Tiny Unbalanced n (n1 = 3, n2 = 30)
# True Mean Shift d = 2.0 -> Compare Power (1 - Beta)!
n1 <- 3; n2 <- 30; d <- 2.0
m1 <- matrix(rnorm(B * n1, mean = d, sd = 1), B, n1)
m2 <- matrix(rnorm(B * n2, mean = 0, sd = 1), B, n2)

p_student_power <- sapply(1:B, function(i) t.test(m1[i,], m2[i,], var.equal = TRUE)$p.value)
p_welch_power   <- sapply(1:B, function(i) t.test(m1[i,], m2[i,], var.equal = FALSE)$p.value)

cat("--- CASE 1: Normal, Equal SD=1, n1=3 vs n2=30 (True d = 2.0) ---\n")
cat("Student's t Power:", round(mean(p_student_power < 0.05) * 100, 1), "% (Beta =", round(mean(p_student_power >= 0.05) * 100, 1), "%)\n")
cat("Welch's t Power:  ", round(mean(p_welch_power < 0.05) * 100, 1),   "% (Beta =", round(mean(p_welch_power >= 0.05) * 100, 1), "%)\n\n")

# CASE 2: Identical Skewed Populations (Exponential, Equal SD=1), n1 = 6 vs n2 = 60
# H0 is TRUE (d = 0) -> Compare Type I Error (Alpha)!
n1 <- 6; n2 <- 60
e1 <- matrix(rexp(B * n1, rate = 1), B, n1)
e2 <- matrix(rexp(B * n2, rate = 1), B, n2)

p_student_alpha <- sapply(1:B, function(i) t.test(e1[i,], e2[i,], var.equal = TRUE)$p.value)
p_welch_alpha   <- sapply(1:B, function(i) t.test(e1[i,], e2[i,], var.equal = FALSE)$p.value)

cat("--- CASE 2: Exponential, Equal SD=1, n1=6 vs n2=60 (H0 True, Nominal Alpha = 5%) ---\n")
cat("Student's t Alpha:", round(mean(p_student_alpha < 0.05) * 100, 1), "%\n")
cat("Welch's t Alpha:  ", round(mean(p_welch_alpha < 0.05) * 100, 1), "% (Lukacs-Geary hits the small n1!)\n")