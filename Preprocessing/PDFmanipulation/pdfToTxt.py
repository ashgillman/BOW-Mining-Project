__author__ = 'Ashley Gillman and Dean Ditton'

import os, fnmatch, re, string
from pdfminer.pdfpage import PDFPage
from pdfminer.pdfinterp import PDFPageInterpreter, PDFResourceManager
from cStringIO import StringIO
from pdfminer.layout import LAParams
from pdfminer.pdfparser import PDFParser
from pdfminer.pdftypes import resolve1
from pdfminer.converter import TextConverter

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

def strip_text_to_words(text):
    text = text.lower()
    pattern = re.compile('[^a-z_]+')
    text = pattern.sub(' ', text)
    text = ' '.join(text.split())
    return text

def pdf_to_words(file):
    text = convert_pdf_to_text(file)
    text = strip_text_to_words(text)
    return text

def is_guttenberg(text):
    return text.startswith("The Project Gutenberg")

def strip_guttenberg(text):
    text = text.split("***")[2] # After *** Start OF ... ***
    text = strip_text_to_words(text)
    return text

def convert_deep(path):
    path = os.sep + os.path.join(*re.split("\\\\|/", path))
    for root, dirnames, filenames in os.walk(path):

        # process PDFs
        for filename in fnmatch.filter(filenames, '*.pdf'):
            text = pdf_to_words(os.path.join(root, filename))
            title = os.path.splitext(os.path.split(filename)[1])[0]
            print(title)
            txtFile = open(os.path.join(root, title + ".txt"), 'w')
            txtFile.write(text)
            txtFile.close()

        # process Guttenberg TXTs
        for filename in fnmatch.filter(filenames, '*.txt'):
            txtFile = open(os.path.join(root, filename), 'r')
            text = txtFile.read()
            txtFile.close()
            if is_guttenberg(text):
                text = strip_guttenberg(text)
                print(filename)
                txtFile = open(os.path.join(root, filename), 'w')
                txtFile.write(text)
                txtFile.close()


path = "/Users/Ash/Dropbox/Uni/2014/CP3300/BOW/data/"
convert_deep(path)