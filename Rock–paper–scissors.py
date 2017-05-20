
# coding: utf-8

# In[ ]:

import random
def game():
    sjb = ['shitou','jiandao','bu']
    count_equal = 0
    count_computer = 0
    count_human = 0
    def ask_input(message):
        while True:
            value= input(message)
            try: 
                value = str(value)
                if value in sjb:
                    break
                else:
                    print("Your input is not valid,please input 'shitou', 'jiandao','bu', Thank you")
            except ValueError as e:
                print("Your input is not valid, please input 'shitou', 'jiandao','bu', Thank you", e)
        return value
    for i in range(3):
        human = ask_input("Your choice is ")
        computer = random.choice(sjb)
        if human == computer:
            count_equal =count_equal +1
            print("Computer's choice is", computer, "Tie")
        elif human == 'shitou' and computer == 'jiandao':
            count_human= count_human+1
            print("Computer's choice is", computer,"human win")
        elif human == 'jiandao' and computer == 'bu':
            count_human = count_human +1
            print("Computer's choice is", computer,"human win")
        elif human == 'bu' and computer == 'shitou':  
            count_human = count_human +1
            print("Computer's choice is", computer,"human win")
        else:
            count_computer = count_computer +1
            print("Computer's choice is", computer,'computer win')
        if count_computer > 2:
            break
    return print("Computer won", count_computer, "rounds","Human won", count_human, "rounds","Tie", count_equal, "rounds")
game()

