from pyswip import Prolog
from pyswip import Functor
import tkinter as tk
import time
import numpy as np
import pandas as pd

FOLDER_PATH = "/home/giulio/Documents/Intelligenza_artificiale/"

def percentile(array, num):
    data = np.array(array)
    ranges = []
    min = data.min()
    max = data.max()
    try:
        val = 100/num
        print(num)
        print(val)
        if val % 1 != 0:
            raise(ValueError(f'Non posso dividere i dati in un numero non intero di gruppi'))
    except(ZeroDivisionError, ValueError) as e:
        print(f'Errore nella divisione: {e}')
    except Exception as e:
        print(f"Errore generico: {e}")
    groups = np.arange(1, num + 1)
    i = 0
    sum = val
    for index in groups:
        if index == 1:
            percentile = np.percentile(data, val)
            ranges.append([min,percentile])
            val += sum
            i += 1
        elif index == groups[-1]:
            prev = ranges[i-1]
            ranges.append([prev[-1],max])
        else:
            prev = ranges[i-1]
            percentile = np.percentile(data, val)
            ranges.append([prev[-1],percentile])
            val += sum
            i += 1
    return ranges

def scegli_tier(val, ranges):
    for index, elem in enumerate(ranges, start=1):
        if index == len(ranges) and elem[0] <= val <= elem[-1]:
            return f'tier_{index}'
        elif elem[0] <= val < elem[-1]:
            return f'tier_{index}'

    # Se il valore è troppo basso, restituisci il tier minimo
    if val < ranges[0][0]:
        return f'tier_1'

    # Se il valore è troppo alto, restituisci il tier massimo
    if val >= ranges[-1][-1]:
        return f'tier_{len(ranges)}'

    # Se il valore non è compreso in nessun range restituisco il tier minimo
    #return f'tier_1'


def get_tier_for_value(attributo, num, value):
    data = pd.read_csv(FOLDER_PATH+"dataset_clean.csv")
    attr_vett = data[attributo].tolist()
    ranges = percentile(attr_vett, num)
    print(ranges)
    tier = scegli_tier(value, ranges)
    return tier

def imposta_range(attributo, num):
    data = pd.read_csv(FOLDER_PATH+"dataset_clean.csv")
    attr_vett = data[attributo].tolist()
    ranges = percentile(attr_vett, num)

    for index, row in data.iterrows():
        tier = scegli_tier(row[attributo], ranges)
        print(f"{index}: {tier}")

def format_value(value):
    output = ""
    if isinstance(value, list):
        output = "[ " + ", ".join([format_value(val) for val in value]) + " ]"
    elif isinstance(value, Functor) and value.arity == 2:
        output = "{0}{1}{2}".format(value.args[0], value.name, value.args[1])
    else:
        output = "{}".format(value)
    return output


def format_result(result):
    result = list(result)
    if len(result) == 0:
        return "false."
    if len(result) == 1 and len(result[0]) == 0:
        return "true."
    output = ""
    for res in result:
        tmpOutput = []
        for var in res:
            tmpOutput.append(var + " = " + format_value(res[var]))
        output += ", ".join(tmpOutput) + " ;\n"
    output = output[:-3] + " ."
    return output


def interroga():
    prolog = Prolog()
    #Determino i valori inseriti dall'utente
    values = [entry.get() for entry in entry_widgets]
    #Indico il file da consultare
    FILE_DA_CONSULTARE = "tree_induction_entropia.pl"
    #Carico il file da consultare
    prolog.consult(FOLDER_PATH+"Apprendimento_NBA/"+FILE_DA_CONSULTARE)
    #Costruisco la query ricavandomi il tier
    query = "["
    for chiave, user_input in zip(attributi_dict.keys(), values):
        if len(user_input) == 0: user_input = 0
        tier_result = get_tier_for_value(chiave, attributi_dict[chiave], float(user_input))
        print(f"Il tier corrispondente per "+chiave+" è: "+tier_result)
        query = query+chiave+"="+tier_result+","
    query = query[:-1]
    query=query+"]"
    print(query)

    print("Inizio l'apprendimento dei sani")
    tempo_inizio=time.time()
    answer = prolog.query("lancia_apprendi(sano).")
    print(format_result(answer))
    tempo_fine=time.time()
    tempo_totale = tempo_fine - tempo_inizio
    print(f"Tempo totale di esecuzione: {tempo_totale} secondi")

    print("Inizio l'apprendimento degli infortunati")
    tempo_inizio = time.time()
    answer = prolog.query("lancia_apprendi(infortunato).")
    print(format_result(answer))
    tempo_fine = time.time()
    tempo_totale = tempo_fine - tempo_inizio
    print(f"Tempo totale di esecuzione: {tempo_totale} secondi")

    print("Interrogo il programma")
    tempo_inizio = time.time()
    answer = prolog.query(("classifica_oggetto(" + query + ", Classe)."))
    print(format_result(answer))
    tempo_fine = time.time()
    tempo_totale = tempo_fine - tempo_inizio
    print(f"Tempo totale di esecuzione: {tempo_totale} secondi")

root = tk.Tk()
root.title("Apprendimento intelligente NBA")
labels = ["Età", "Altezza", "Peso", "Partite giocate", "Minuti a partita",
          "Impiego attivo (%)", "Ritmo di gioco", "Possessi a partita",
          "Tiri tentati a partita", "Attacchi al canestro a partita",
          "Miglia percorse a partita", "Velocità media",
          "Tiri in sospensione a partita", "Frequenza di palleggio",
          "Media dribbling per tocco", "Tocchi spalle a canestro",
          "Tocchi nel pitturato"]


entry_widgets = []


attributi_dict = {
    'age': 4,
    'height': 4,
    'weight': 5,
    'gp': 5,
    'min': 4,
    'usg_pct': 5,
    'pace': 4,
    'poss': 10,
    'fga_pg': 5,
    'drives': 10,
    'dist_miles': 5,
    'avg_speed': 4,
    'pull_up_fga': 10,
    'avg_sec_per_touch': 5,
    'avg_drib_per_touch': 8,
    'post_touches': 10,
    'paint_touches': 10
}

for i, label_text in enumerate(labels):
    label = tk.Label(root, text=label_text, font=("Arial", 12))

    label.grid(row=i, column=0, padx=10, pady=5, sticky="w")
    entry = tk.Entry(root, width=10, font=("Arial", 12))
    entry.grid(row=i, column=1, padx=10, pady=5)
    entry_widgets.append(entry)

button = tk.Button(root, text="Costruisci la query", command=interroga, font=("Arial", 12))
button.grid(row=len(labels), column=1, pady=10)

root.mainloop()
