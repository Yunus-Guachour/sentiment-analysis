#Split_csv_into_multiple_files
import csv

 

chunk = []

row_count = 0

file_number = 1

header = ''

 

with open('C:/Users/yugua8/OneDrive - IKEA/Desktop/python/GeoSegmentatie_2024Q2.csv', 'r') as file:

    reader = csv.reader(file)

    for row in reader:

        if header == '':

            header = row

            continue

 

        chunk.append(row)

        row_count += 1

        if row_count >= 480000:

            # Writing a new file with proper newline handling

            with open(f'C:/Users/yugua8/OneDrive - IKEA/Desktop/python/GeoSegmentatie_2024Q2-{file_number}.csv', 'w', newline='') as new_file:

                writer = csv.writer(new_file)

                writer.writerow(header)

                writer.writerows(chunk)

            chunk = []

            file_number += 1

            row_count = 0

 

    if chunk:

        # Writing the final chunk

        with open(f'C:/Users/yugua8/OneDrive - IKEA/Desktop/python/GeoSegmentatie_2024Q2-{file_number}.csv', 'w', newline='') as new_file:

            writer = csv.writer(new_file)

            writer.writerow(header)

            writer.writerows(chunk)