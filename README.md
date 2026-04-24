# ✈️ Flight Data Parser

A lightweight data parsing tool that converts **unstructured flight search results** into clean, structured, analysis-ready datasets.

---

## 📌 Overview

Flight search outputs are often messy and inconsistent — with multiple airlines, varying baggage formats, and irregular layouts.

This project solves that problem by transforming raw input (HTML/text) into structured data that can be easily analyzed or visualized.

---

## 🚀 Features

- Extracts key flight details:
  - Airline
  - Flight number
  - Departure & arrival times
  - Duration
  - Price
  - Baggage category (0kg, 20kg, 30kg+)

- Handles inconsistent and unstructured input formats  
- Categorizes flights by:
  - Time of day (Morning / Afternoon / Evening)
  - Direct vs. Stop flights  

- Converts messy raw data into clean tabular format  

---

## 🧠 Use Case

This tool is useful for:
- Data analysts working with aviation datasets  
- Operations teams handling fare comparisons  
- Anyone dealing with repetitive manual data cleaning  

---

## ⚙️ Tech Stack

- **R**
- **Shiny**
- **dplyr**
- **stringr**

---

## 📥 Input

- Raw/unstructured flight search results  
- Typically copied HTML or text-based content  

---

## 📤 Output

A structured dataset with:

| Airline | Flight No | Departure | Arrival | Duration | Price | Baggage | Category |
|--------|----------|----------|--------|----------|------|----------|----------|

---

## 📸 Demo

_Add screenshots or screen recording here_

---

## 🛠️ How It Works

1. Paste raw flight search data into the tool  
2. Parser identifies patterns and extracts relevant fields  
3. Data is cleaned, categorized, and structured  
4. Output is ready for analysis or export  

---

## 🔒 Disclaimer

This project is for **educational and demonstration purposes only**.

- It does **not perform web scraping**
- It does **not connect to any external websites or APIs**
- It only processes **user-provided input data**
- No proprietary or sensitive data is included  

---

## 🔮 Future Improvements

- Support for multiple airline formats dynamically  
- File upload (CSV/HTML) instead of manual input  
- Improved pattern recognition for edge cases  
- Dashboard for visualization and analytics  

---

## 🤝 Contributions

Suggestions, improvements, and feedback are welcome!

---

## 📬 Contact

If you’d like to connect or discuss improvements, feel free to reach out via LinkedIn.
