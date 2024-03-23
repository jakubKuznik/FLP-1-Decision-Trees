# Decision Trees   
## Faculty: BUT FIT, Course: FLP 

Name: Jakub Kuznik  
Login: xkuzni04  

## Execution:
### Task 1
The program classifies new data based on an existing decision tree.

`flp-fun -1 <file-with-three> <file-with-new-data>`

File with three
``` 
Node: 0, 5.5
  Leaf: TridaA
  Node: 1, 3.0
    Leaf: TridaB
    Leaf: TridaC
```
File with new data 
```
2.4,1.3
6.1,0.3
6.3,4.4
```
Output:
```
TridaA
TridaB
TridaC
```
### Task 2 
`flp-fun -2 <file with training data>`  
The Program creates a decision tree based on the training data.   

In this implementation, the Gini index is calculated without pruning. During each iteration, all possible splits are evaluated, and the one resulting in the highest information gain is selected. When printed at the end of the program, values are rounded to one decimal place.

File with training data 
```
2.4,1.3,TridaA
6.1,0.3,TridaB
6.3,4.4,TridaC
2.9,4.4,TridaA
3.1,2.9,TridaB
```
Output 
```
  Node: 0, 3.0
    Leaf: TridaA
    Node: 0, 6.2
      Leaf: TridaB
      Leaf: TridaC
```
