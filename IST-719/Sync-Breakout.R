# Break-out
dataset <- read.csv('/Users/venkatasharatsripada/Downloads/dataplot.csv')

# Dot plot
plot(dataset$Sales, pch=16, ylab = "Sales")

# Filter for State = Texas
dataset_texas <- dataset[dataset$State == 'Texas', ]

plot(dataset_texas$Sales, pch=16, ylab = "Texas Sales")


my.letters <- sample(letters[7:9], size = 50, replace =T)
barplot(table(my.letters))

df <- data.frame()

for row in df1.values:
  region = row[0]
  rep = row[1]
  year = row[2]
  sold = row[3]
  keys_list = []
  for item in d['children']:
    keys_list.append(item['region'])
  if not region in keys_list:
    d['children'].append({"region": region, "children":[{"rep": rep,"year": year,"sold": sold}]})
  else:
    d['children'][keys_list.index(region)]['children'].append({"region": region,"rep": rep,"sold": sold})