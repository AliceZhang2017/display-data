
# coding: utf-8

# In[9]:

def ask_input(message):
    try:
        value1 = input(message)
        value1 = int(value1)
    except ValueError as e:
        print("You input a bad number:", e)
        sys.exit(-1)
    return value1
def guess_year():
    year = ask_input("Please input a year")
    if year % 4 ==0:
        if year % 100 ==0 and year%400 != 0:
            print("This is not a leap year")
        else:
            print("This is a leap year")
    else:
        print("This is not a leap year")
guess_year()
    

