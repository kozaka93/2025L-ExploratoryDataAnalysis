from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import requests
from bs4 import BeautifulSoup
import time

#options = Options()
#options.add_argument('--headless=new')
chr = Service(r'C:\Users\tomus\Desktop\Filmweb_Project\Selenium\chromedriver.exe')
driver = webdriver.Chrome(service=chr,)
url = r'https://www.imdb.com/search/title/?title_type=feature&sort=num_votes,desc'

def scroll(url,n):
    driver.get(url)
    wait = WebDriverWait(driver, 10)
    for i in range(n):
        click_button = wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR, ".ipc-see-more__button")))
        driver.execute_script("arguments[0].click();", click_button)
        time.sleep(2)
    html = driver.execute_script("return document.getElementsByTagName('html')[0].innerHTML")
    return html

def a(html):
    soup = BeautifulSoup(html, 'html.parser')
    all_links = [a.get('href') for a in soup.find_all('a') if a.get('href')]
    film_links = [link for link in all_links if link.startswith('/title/')]
    filtered_links = [link for link in film_links if 'sr_t' not in link]
    return filtered_links

x = scroll(url,200)
lisy = a(x)
with open('links_imdb','w') as file:
    for lis in lisy:
        file.write('https://www.imdb.com/'+ lis + '\n')