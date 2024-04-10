from pyswip import Prolog
from pyswip import Functor
from tkinter import ttk
import tkinter as tk
import time
import numpy as np
import pandas as pd
import os

FILE_DA_CONSULTARE = "tree_induction_entropia.pl"
APPRENDIMENTO = False

def cambiaPercorso(nome_file):
    with open(os.getcwd() + '/Apprendimento_NBA/' + nome_file, 'r') as file, open(os.getcwd() + '/Apprendimento_NBA/' + nome_file + '.tmp', 'w') as file_temp:
    # Itera ogni riga del file
        for line in file:
            if line.startswith('file_output'):
                line = "file_output('"+os.getcwd() + "/Apprendimento_NBA/file_output.txt').\n"
            elif line.startswith('file_albero'):
                line = "file_albero('"+os.getcwd() + "/Apprendimento_NBA/albero.pl').\n"
            file_temp.write(line)

    os.remove(os.getcwd()+'/Apprendimento_NBA/'+nome_file)  # Rimuove il file originale
    os.rename(os.getcwd()+'/Apprendimento_NBA/'+nome_file + '.tmp', os.getcwd()+'/Apprendimento_NBA/'+nome_file) #Rinomina

def on_combobox_change(event):
    global FILE_DA_CONSULTARE
    selected_value = combobox.get()
    if selected_value == "Gini": FILE_DA_CONSULTARE = "tree_induction_gini.pl"
    else: FILE_DA_CONSULTARE = "tree_induction_entropia.pl"
    print(f"Hai selezionato: {FILE_DA_CONSULTARE}")

def percentile(array, num):
    data = np.array(array)
    ranges = []
    min = data.min()
    max = data.max()
    try:
        val = 100/num
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
        elif elem[0] <= val <= elem[-1]:
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
    data = pd.read_csv(os.getcwd()+"/dataset_clean.csv")
    attr_vett = data[attributo].tolist()
    ranges = percentile(attr_vett, num)
    tier = scegli_tier(value, ranges)
    return tier

def imposta_range(attributo, num):
    data = pd.read_csv(os.getcwd()+"/dataset_clean.csv")
    attr_vett = data[attributo].tolist()
    ranges = percentile(attr_vett, num)

    for index, row in data.iterrows():
        tier = scegli_tier(row[attributo], ranges)

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
    global APPRENDIMENTO
    cambiaPercorso('tree_induction_entropia.pl')
    cambiaPercorso('tree_induction_gini.pl')
    prolog = Prolog()
    #Determino i valori inseriti dall'utente
    values = [entry.get() for entry in entry_widgets]
    if values[5] != '': values[5] = str(float(values[5]) / 100 )#Normalizzo la percentuale
    #Carico il file da consultare
    prolog.consult(os.getcwd()+"/Apprendimento_NBA/"+FILE_DA_CONSULTARE)
    #Costruisco la query ricavandomi il tier
    query = "["
    for chiave, user_input in zip(attributi_dict.keys(), values):
        if len(user_input) == 0: user_input = 0
        tier_result = get_tier_for_value(chiave, attributi_dict[chiave], float(user_input))
        query = query+chiave+"="+tier_result+","
    query = query[:-1]
    query=query+"]"

    if not APPRENDIMENTO:
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
	    
        print("Lancio l'induzione dell'albero.")
        prolog.consult(os.getcwd()+"/Apprendimento_NBA/"+FILE_DA_CONSULTARE)
        tempo_inizio=time.time()
        answer = prolog.query("lancia_induzione(Albero).")
        matrice = format_result(answer)
        tempo_fine=time.time()
        tempo_totale = tempo_fine - tempo_inizio
        print(f"Tempo totale di esecuzione: {tempo_totale} secondi")
        APPRENDIMENTO = True

    print("Interrogo il programma")
    tempo_inizio = time.time()
    answer = prolog.query(("classifica_oggetto(" + query + ", Classe)."))
    risposta = format_result(answer).split(';')
    print(risposta[0])
    tempo_fine = time.time()
    tempo_totale = tempo_fine - tempo_inizio
    print(f"Tempo totale di esecuzione: {tempo_totale} secondi")

def potatura():
    cmdpotatura = Prolog()
    cmdpotatura.consult(os.getcwd()+"/Apprendimento_NBA/potatura.pl")
    if os.path.exists(os.getcwd()+"/Apprendimento_NBA/albero.pl"):
        answer = cmdpotatura.query("set_prolog_stack(global, limit(3*10**9)).")
        print(format_result(answer))
        answer = cmdpotatura.query("lancia(MigliorAlbero, Punteggio).")
        print(format_result(answer))
    else: print("Albero mancante: eseguire prima una query.")

root = tk.Tk()
root.title("Apprendimento intelligente NBA")
labels = ["Età", "Altezza", "Peso", "Partite giocate", "Minuti a partita",
          "Impiego attivo (%)", "Ritmo di gioco", "Possessi per stagione",
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
    'avg_drib_per_touch': 10,
    'post_touches': 4,
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

button2 = tk.Button(root, text="Potatura", command=potatura, font=("Arial", 12))
button2.grid(row=len(labels)+1, column=1, pady=10)

label=tk.Label(root, text="Metodo di induzione:", font=("Arial",12))
label.grid(padx=5, pady=5, sticky="w")
combobox = ttk.Combobox(root, values=["Entropia", "Gini"])
combobox.current(0)
combobox.bind("<<ComboboxSelected>>", on_combobox_change)
combobox.grid(column=0, padx=5, pady=5, sticky="w")
icona = tk.PhotoImage(file='icon.png')
root.iconphoto(True, icona)
root.mainloop()
