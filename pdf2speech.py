#import libraries
from tkinter import *    
from gtts import gTTS    
from playsound import playsound

import PyPDF2 
import textract
import nltk
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords


nltk.download('punkt')
nltk.download('stopwords')

#Here Tk() is used to initialize tkinter which will be used for GUI
root = Tk()
root.geometry('500x200')     
root.resizable(0,0)
root.config(bg = 'dark blue')    
root.title('Text to Speech') 

#iconbitmap()method is used to set the icon of the window/frame.The bitmap must be an ico type, but not png or jpg type, otherwise  the image will not display as the icon.
#root.iconbitmap('C:/Users/User/Desktop/Competitive coding/Week 7/text to speech/Text-to-speech-icon.ico')

#Label() here is a widget, which is used to display one or more than one line of text that users can’t able to modify.
#pack() method is used to declare the position of the widget
Label(root, text = 'TEXT TO SPEECH' , font='arial 20 bold' , bg ='sky blue').pack()
#place() method is used to organize widgets by placing them in a specific position(for example x=50,y=40)
Label(root, text ='Enter Text', font ='Helvetica', bg ='sky blue').place(x=20,y=60)

# ************************************************************************************************************************************************
# GENERATING THE TEXT AS INPUT

## Option #1
#Msg is a string type variable!
Msg = StringVar()


## Option #2
filename = '/Users/jpolanco/Desktop/pruebas/OpenAI/text2speech/input/butler 1.pdf' 

# open allows you to read the file.
pdfFileObj = open(filename,'rb')

# The pdfReader variable is a readable object that will be parsed.
pdfReader = PyPDF2.PdfReader(pdfFileObj)

# Discerning the number of pages will allow us to parse through all the pages.
num_pages = len(pdfReader.pages)
count = 0
text = ""

# The while loop will read each page.
while count < num_pages:
    pageObj = pdfReader.pages[count]
    count +=1
    text += pageObj.extract_text()

# This if statement exists to check if the above library returned words. It's done because PyPDF2 cannot read scanned files.
if text != "":
   text = text
# If the above returns as False, we run the OCR library textract to #convert scanned/image based PDF files into text.
else:
   text = textract.process(fileurl, method='tesseract', language='eng')

# The word_tokenize() function will break our text phrases into individual words.
tokens = word_tokenize(text)

# We'll create a new list that contains punctuation we wish to clean.
punctuations = ['(',')',';',':','[',']',',']

# We initialize the stopwords variable, which is a list of words like "The," "I," "and," etc. that don't hold much value as keywords.
stop_words = stopwords.words('english')

# We create a list comprehension that only returns a list of words that are NOT IN stop_words and NOT IN punctuations.
keywords = [word for word in tokens if not word in stop_words and not word in punctuations]

#print(keywords)
print("-------------------------------")
print(text)
print("-------------------------------")

Msg = text

# ************************************************************************************************************************************************

#Entry() widget is used to accept single-line text strings from the user textvariable is used to bring the current text to entry widget
entry_field = Entry(root,textvariable = Msg, width ='50')
entry_field.place(x=20 , y=100)

#user define function to convert text to speech
def Text_to_speech():
    Message = entry_field.get()
    speech = gTTS(text = Message, lang='en', slow=False)  
    speech.save('/Users/jpolanco/Desktop/pruebas/OpenAI/text2speech/output/test_1.mp3')  
    # saving the converted audio file in a mp3 file
    playsound('test_1.mp3')    

#function to exit
def Exit():
    root.destroy()

#Reset function 
def Reset():
    Msg.set("")

#Button widget is used to display buttons on the window!
Button(root, text = "Play" , font = 'arial 15 bold', command = Text_to_speech, bg = 'sky blue', width =4).place(x=25, y=140)
Button(root,text = 'Exit',font = 'arial 15 bold' , command = Exit, bg = 'sky blue').place(x=100,y=140)
Button(root, text = 'Reset', font='arial 15 bold', command = Reset, bg= 'sky blue').place(x=175 , y =140)

#infinite loop to run program
root.mainloop()