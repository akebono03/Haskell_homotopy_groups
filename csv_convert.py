import sqlite3
import csv

# SQLiteデータベースファイルを作成（存在しない場合）
db_filename = 'sphere.db'
connection = sqlite3.connect(db_filename)
cursor = connection.cursor()

# テーブルを作成
cursor.execute('''CREATE TABLE IF NOT EXISTS my_table (
                    k INTEGER,
                    n INTEGER,
                    id INTEGER,
                    orders INTEGER,
                    generator TEXT,
                    P TEXT,
                    P_coe TEXT,
                    E TEXT,
                    E_coe TEXT,
                    H TEXT,
                    H_coe TEXT,
                    Element TEXT,
                    gen_coe TEXT,
                    hyouji TEXT,
                    orders2 INTEGER
                    )''')

# コミットして変更を保存
connection.commit()

# CSVファイルからデータを読み込み、データベースに挿入
csv_filename = 'sphere.csv'

with open(csv_filename, 'r', newline='', encoding='utf-8') as csvfile:
    csvreader = csv.reader(csvfile)
    next(csvreader)  # ヘッダーをスキップ

    for row in csvreader:
        cursor.execute('''INSERT INTO my_table (k, n, id, orders, generator, P, P_coe, E, E_coe, H, H_coe, Element, gen_coe, hyouji, orders2)
                          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)''', (row[0], row[1], row[2], row[3], row[4], row[5], row[6], row[7], row[8], row[9], row[10], row[11], row[12], row[13], row[14]))

# コミットして変更を保存
connection.commit()

# データベース接続を閉じる
connection.close()



# import csv

# # CSVファイルの読み込み
# with open('sphere2.csv', 'r', newline='', encoding='utf-8') as csvfile:
#   reader = csv.reader(csvfile)
#   data = list(reader)

# # データの変換
# for row in data:
#   for i in range(3):
#     try:
#       # 小数点を持つ文字列を整数に変換
#       row[i] = str(int(float(row[i])))
#     except ValueError:
#       # 変換できない場合はそのまま残す
#       pass

# # 変換されたデータを新しいCSVファイルに書き込む
# with open('sphere3.csv', 'w', newline='', encoding='utf-8') as csvfile:
#   writer = csv.writer(csvfile)
#   writer.writerows(data)




# import csv

# # ギリシャ文字をTeX表記に変換する辞書
# greek_to_tex = {
#   'α': r'\alpha',
#   'β': r'\beta',
#   'γ': r'\gamma',
#   'δ': r'\delta',
#   'ε': r'\epsilon',
#   'ζ': r'\zeta',
#   'η': r'\eta',
#   'θ': r'\theta',
#   'ι': r'\iota',
#   'κ': r'\kappa',
#   'λ': r'\lambda',
#   'μ': r'\mu',
#   'ν': r'\nu',
#   'ξ': r'\xi',
#   'ο': r'\omicron',
#   'π': r'\pi',
#   'ρ': r'\rho',
#   'σ': r'\sigma',
#   'τ': r'\tau',
#   'υ': r'\upsilon',
#   'φ': r'\phi',
#   'χ': r'\chi',
#   'ψ': r'\psi',
#   'ω': r'\omega'
#   # 'Α': r'A',
#   # 'Β': r'B',
#   # 'Γ': r'\Gamma',
#   # 'Δ': r'\Delta',
#   # 'Ε': r'E',
#   # 'Ζ': r'Z',
#   # 'Η': r'H',
#   # 'Θ': r'\Theta',
#   # 'Ι': r'I',
#   # 'Κ': r'K',
#   # 'Λ': r'\Lambda',
#   # 'Μ': r'M',
#   # 'Ν': r'N',
#   # 'Ξ': r'\Xi',
#   # 'Ο': r'O',
#   # 'Π': r'\Pi',
#   # 'Ρ': r'P',
#   # 'Σ': r'\Sigma',
#   # 'Τ': r'T',
#   # 'Υ': r'\Upsilon',
#   # 'Φ': r'\Phi',
#   # 'Χ': r'X',
#   # 'Ψ': r'\Psi',
#   # 'Ω': r'\Omega'
# }

# # ギリシャ文字をTeX表記に変換する関数
# def convert_greek_to_tex(text):
#   for greek, tex in greek_to_tex.items():
#     text = text.replace(greek, tex)
#   return text

# # CSVファイルの読み込みと変換
# # with open('sphere.csv', 'r', newline='') as csvfile:
# # with open('sphere.csv', 'r', newline='', encoding='cp932') as csvfile:
# with open('sphere.csv', 'r', newline='', encoding='utf-8') as csvfile:
#   reader = csv.reader(csvfile)
#   data = list(reader)

# # データの変換
# for row in data:
#   for i in range(len(row)):
#     row[i] = convert_greek_to_tex(row[i])

# # 変換されたデータを新しいCSVファイルに書き込む
# with open('sphere2.csv', 'w', newline='', encoding='utf-8') as csvfile:
#   writer = csv.writer(csvfile)
#   writer.writerows(data)
