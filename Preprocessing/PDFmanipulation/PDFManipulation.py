__author__ = 'Dean'


import glob
import csv
from pdfminer.pdfpage import PDFPage
from pdfminer.pdfinterp import PDFPageInterpreter, PDFResourceManager
from cStringIO import StringIO
from pdfminer.layout import LAParams
from pdfminer.pdfparser import PDFParser
from pdfminer.pdftypes import resolve1
from pdfminer.converter import TextConverter


'''"C:\Users\Dean\Documents\TestPDF's" +'''

def get_file_paths():
    sysPath = "/*.pdf"
    user_input = raw_input("Enter the document paths:")
    fullPath = user_input + sysPath
    fileArray = glob.glob(fullPath)
    return fileArray

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

'''
def get_title_metadata(file_path):
    fp = open('mypdf.pdf', 'rb')
    parser = PDFParser(fp)
    doc = PDFDocument()
    parser.set_document(doc)
    doc.set_parser(parser)
    doc.initialize()

    print doc.info        # The "Info" metadata

    if 'Metadata' in doc.catalog:
        metadata = resolve1(doc.catalog['Metadata']).get_data()
        print xmp_to_dict(metadata)

'''

def str_processing(file):
    word_list = []
    strfile = convert_pdf_to_text(file)
    replace_chars = ['-', ':', ',', '.', '(', ')', "1", '2', '3', '4', '5', '6', '7', '8', '9', '0']
    for i in replace_chars:
        strfile = strfile.replace(i, ' ')
    strfile = strfile.lower()
    processed_information = strfile.split()
    word_list.append(processed_information)
    return word_list


if __name__ == "__main__":
    file_path_array = get_file_paths()
    for f in file_path_array:
        text = str_processing(f)
        #title = get_title_metadata(f)
        csv_file = open(title, wb)
        writer = csv.writer(csv_file)
        writer.writerows(text)
        csv_file.close()


