# %%
# # import requests
import sys
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.alert import Alert
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import pandas as pd
import re  # Regular expressions!
import time
import os
import shutil
import glob
from datetime import datetime
import subprocess

# Ensure the network drive is mounted
try:
    subprocess.run(["net", "use", "Z:"], check=True, shell=True)
except subprocess.CalledProcessError:
    print("⚠️ Could not verify Z: drive mapping. Trying to reconnect...")
    # You can supply credentials if needed:
    # subprocess.run(["net", "use", "Z:", "\\\\SFP.IDIR.BCGOV\\Science", "/persistent:yes"], shell=True)


# %%
def wait_for_download(directory, file_pattern, timeout=60, check_interval=5):
    
    start_time = time.time()

    while time.time() - start_time < timeout:
        files = glob.glob(os.path.join(directory, file_pattern))
        if files:
            file_path = files[0]
            initial_size = -1
            while initial_size != os.path.getsize(file_path):
                initial_size = os.path.getsize(file_path)
                time.sleep(check_interval)
            return file_path
        time.sleep(check_interval)

    return None


# %%
## Read in options file that has log-in info for metabase
my_opts_filename = (
    "C:\\Users\\KHACKETT\\Documents\\R_Projects\\Long_Term_Projects\\AIS_IMDP_Figures\\Options.csv"
    #sys.argv[1]
)

print(my_opts_filename)

my_opts = pd.read_csv(my_opts_filename)

the_year = int(my_opts["year"].iloc[0])
# Create the WebDriver instance outside the loop
driver = webdriver.Chrome()

#2025 dat 
#url = "https://metabase-7068ad-prod.apps.silver.devops.gov.bc.ca/question/466-2025-mussel-summary-csv-export"
# 2026 data 
url = "https://metabase-7068ad-prod.apps.silver.devops.gov.bc.ca/question/510-2026-mussel-summary-csv-export"
#url = sys.argv[2]
url2 = "https://metabase-7068ad-prod.apps.silver.devops.gov.bc.ca/question/467-get-blowby-table"

# Define the path to the Downloads folder
downloads_folder = os.path.expanduser("~/Downloads")

now = datetime.now()

if now.month < 3 or now.month > 11:
    the_year = now.year - 1
else:
    the_year = now.year

#%% 
for the_url in [url, url2]:
    
    # Define the file name
    if the_url == url:
        file_pattern = re.escape(str(the_year)) + r'_mussel_summary_csv_export.*\.csv$'
    if the_url == url2:
        file_pattern = r'get_blowby_table.*\.csv$'

    # Does the file already exist in downloads folder? If so, skip download section. If not, download it.
    # url is the flat file - lots of things about boats inspectors etc 60+ cols
    # url2 blowby table - owner that doesn't stop for whatever reason - data is useful for missed inspections
    if the_url == url:
        file_downloaded_already = [f for f in os.listdir(downloads_folder) if re.search(r'_mussel_summary_csv_export.*\.csv$', f)]
    elif the_url == url2:
        file_downloaded_already = [f for f in os.listdir(downloads_folder) if re.search(r'get_blowby_table_.*\.csv$', f)]

    if file_downloaded_already:
        print("File was found - no need to download!")

    if file_downloaded_already == []:
        # File not yet downloaded. Do it now!
        if(driver.current_url != the_url):
            driver.get(the_url)

        # If we're dealing with the first download URL, we'll need to sign in.
        if the_url == url:
            WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.NAME, "username")))

            username_slot = driver.find_element(By.NAME, "username")
            password_slot = driver.find_element(By.NAME, "password")

            username_slot.send_keys(my_opts["metabase_login"])
            password_slot.send_keys(my_opts["metabase_password"])

            # Find 'submit' button.
            buttons = driver.find_elements(By.TAG_NAME, "button")
            buttons[0].click()

            time.sleep(30)

        # Find the download button.
        WebDriverWait(driver, 20).until(
            EC.presence_of_element_located((By.CLASS_NAME, "Icon-download"))
        )
        download_button = driver.find_element(By.CLASS_NAME, "Icon-download")

        download_button.click()

        # New popup
        # Wait for download popup
        WebDriverWait(driver, 10).until(
            EC.presence_of_element_located(
                (By.CSS_SELECTOR, '[data-element-id="mantine-popover"]')
            )
        )

# Wait for and click Download button
        download_button = WebDriverWait(driver, 10).until(
            EC.element_to_be_clickable(
                (By.CSS_SELECTOR, '[data-testid="download-results-button"]')
            )
        )

        
        download_button.click()
        

        

        # Wait for 3 minutes for download, just in case it takes that long.
        if the_url == url:
            time.sleep(30)
        else:
            time.sleep(30)

    # Wait until the download of this file is complete.

    if the_url == url:
        file_downloaded = [f for f in os.listdir(downloads_folder) if re.search(r'_mussel_summary_csv_export.*\.csv$', f)]
    elif the_url == url2:
        file_downloaded = [f for f in os.listdir(downloads_folder) if re.search(r'get_blowby_table_.*\.csv$', f)]

    if file_downloaded:

        downloaded_file = file_downloaded[0]

        # Define the path to the network drive folder
        network_drive_folder = r"Z:\2 SCIENCE - Invasives\SPECIES\Zebra_Quagga_Mussel\Operations\Watercraft Inspection Data\Raw inspection data for sharing (all years)\Clean files all years"

        # Define the new file name
        if the_url == url:
            new_file_name = f"metabase_{str(now.year)}.csv"
        else:
            new_file_name = f"metabase_blowby_table_2024_onwards.csv"

        old_file_path = os.path.join(downloads_folder, downloaded_file)
        new_file_path = os.path.join(downloads_folder, new_file_name)
        
        # Rename the file
        if glob.glob(new_file_path):
            os.remove(new_file_path)
        os.rename(old_file_path, new_file_path)
        print(f"Renamed file to {new_file_path}")

        # LAN folder path plus file name
        lan_file_path = os.path.join(network_drive_folder, new_file_name)

        # Remove the old version of this file that's on the J: drive, if it exists.
        old_file_on_lan = glob.glob(lan_file_path)

        if old_file_on_lan != []:
            # Pull out the full path to the old metabase summary file.
            old_file_on_lan = old_file_on_lan[0]
            # Delete the old file currently in the LAN folder
            os.remove(old_file_on_lan)
            # Copy over the minty-fresh metabase summary file, from C:/.../Downloads to the LAN folder (J: for me)
            shutil.copyfile(new_file_path, old_file_on_lan)
            # Remove the metabase summary from the local Downloads folder
            os.remove(new_file_path)
        else:
            shutil.copyfile(src=new_file_path, dst=lan_file_path)
            os.remove(new_file_path)


print("Finished downloading metabase file and blowbys tracker (2024 onwards) from the website! Transferring to R now.")
driver.quit()

# %%
