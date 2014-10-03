__author__ = 'Dean'


import glob
from pdfminer.pdfpage import PDFPage
from pdfminer.pdfinterp import PDFPageInterpreter, PDFResourceManager
from cStringIO import StringIO
from pdfminer.layout import LAParams
from pdfminer.converter import TextConverter

def get_file_paths():
    sysPath = "C:\Users\Dean\Documents\TestPDF's" + "\*.pdf"
    fileArray = glob.glob(sysPath)
    for f in fileArray:
        try:
            return fileArray
        except ValueError:
            print 'big error dude'
        except IndexError:
            print "File Name incorrect"

def convert_pdf_to_text(path):
    resourceManager = PDFResourceManager()
    retstr = StringIO()
    codec = 'utf-8'
    laparams = LAParams()
    device = TextConverter(resourceManager, retstr, codec=codec, laparams=laparams)
    fp = file(path, 'rb')
    interpreter = PDFPageInterpreter(resourceManager, device)
    password = ""
    maxPages = 0
    caching = True
    pageNumbers = set()
    for page in PDFPage.get_pages(fp, pageNumbers, maxpages=maxPages, password=password,caching=caching, check_extractable=True):
        interpreter.process_page(page)
    fp.close()
    device.close()
    str = retstr.getvalue()
    retstr.close()
    return str


def str_processing():
    path_array = get_file_paths()
    word_list = []
    for file in path_array:
        strfile = convert_pdf_to_text(file)
        replace_chars = ['-', ':', ',', '.', '(', ')', "1", '2', '3','4', '5', '6', '7', '8', '9', '0']
        for i in replace_chars:
            strfile = strfile.replace(i, ' ')
        strfile = strfile.lower()
        processed_information = strfile.split()
        word_list.append(processed_information)
    print(word_list)


if __name__ == "__main__":
    str_processing()
