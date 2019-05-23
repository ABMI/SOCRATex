
### regexpr for de-identification

# labelled as .. 
(labelled as ".+")|(labelled as "")

# English name
//[a-zA-Z]+
  
# Slide number
[[a-zA-Z][0-9]+\-[0-9]+]
([a-zA-Z][0-9]+\-[0-9]+.+)

# Date
[0-9]+/[0-9]+/[0-9]+
  