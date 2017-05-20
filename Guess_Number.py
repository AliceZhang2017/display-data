
# coding: utf-8

# In[5]:

from random import randint
def guess_number():
    def ask_num(message):
        try: 
            value= int(input(message))
        except ValueError as e:
            print("You put a bad number", e)
        return value
    start_num = ask_num("Input the start number ")
    Stop_num = ask_num("Input the stop number ")
    m = ask_num("Guess a number ")
    g = randint(start_num,Stop_num)
    for i in range(20):
        if m > g:
            print("Your number is large than the random number")
        elif m==g:
            print("You are right!")
            break
        else:
            print("Your number is less than the random number")
guess_number()


# In[ ]:



