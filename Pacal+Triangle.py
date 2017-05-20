
# coding: utf-8

# # Pascal Triangle

# In[1]:

def pascal_triangle(num):
    row = [1]
    for i in range(int(num)):
        print(row)
        x = row[:]
        print("x =", x)
        for j in range(1,len(row)):
            row[j] = x[j] + x[j-1]
        row.append(1)
        print("row = ", row)

pascal_triangle(5)

#pascal_triangle[1]



# In[ ]:
