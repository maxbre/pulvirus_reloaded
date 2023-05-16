Introduzione
========================================================
author: Dr. Raffaele Morelli
width: 1200
height: 800
font-family: 'Serif'


Il progetto
========================================================

L'Obiettivo 1 intende quantificare, al netto della variabilità indotta dalle condizioni meteorologiche, l'entità della riduzione della concentrazione dei principali inquinanti determinata dai provvedimenti adottati per ridurre la diffusione del contagio, utilizzando come base i dati rilevati dalle stazioni di monitoraggio nel corso del 2020, e, per confronto, le serie storiche degli anni precedenti. 

Per ulteriori dettagli visitare la pagina <https://www.pulvirus.it/index.php/obiettivi/obiettivo-1/>.



========================================================

Gli inquinanti presi in esame sono NO<sub>2</sub>, PM<sub>10</sub>, PM~2.5, O<sub>3</sub> e CO, rilevati nelle stazioni di monitoraggio della qualità dell'aria in Italia e per la valutazione dell'effetto del lockdown sulla variazione della concentrazione degli inquinanti, sono stati utilizzati i modelli statistici additivi generalizzati (GAM) che consentono di normalizzare, a livello meteorologico, le serie storiche pluriennali delle concentrazioni degli inquinanti in atmosfera e di valutare il contributo nel tempo di specifiche variabili esplicative di tipo numerico o categoriale.

Le informazioni utilizzate sono state estratte dal database nazionale InfoARIA nel quale vengono raccolti e archiviati dati e metadati di qualità dell'aria trasmessi dalle ARPA/APPA, secondo quanto previsto dalla Direttiva 2008/50/CE, dal D.Lgs 155/2010 di recepimento e dalla Decisione 2011/850/CE. Ogni ARPA/APPA applica sui dati un rigoroso protocollo basato su successivi livelli di validazione, prima di pervenire alla pubblicazione finale.

Corso-00
========================================================

Breve introduzione ad RStudio IDE

Corso 01
========================================================

1. caricamento/scrittura file testo 
2. operazioni con purrr
3. connessioni db
4. esempi con dati inquinamento NO[2]
5. nuove variabili
6. processo validazione delle serie
7. file RDS
8. esempio di modello su tutte le stazioni


Corso 02
========================================================

1. costruzione stringhe modelli 
2. caricamento e gestione variabili meteo 
3. applicazione modelli sulle stazioni valide
4. estrazione indici di valutazione bontà modello
5. scelta variabili più performanti
6. avanzamento a modelli N-variati


Corso 03
========================================================

1. funzioni in R
2. esempio per stazione con funzione di ricorsione
3. file log
4. scelta modello e "pruning" variabili non significative
5. algoritmo di scelta delle variabili


Corso 04 - 05 - 05a
========================================================

1. utilizzo di Rscript e dei parametri da CLI
2. primo esempio di scelta del modello 
3. variabili in enviroment
4. salvataggio sui file RDS/RData
5. il pacchetto mgcViz


Corso 06 - 06a - 07
========================================================

1. validazione modelli
2. valutazione incertezza modelli
3. utilizzo di matrici e liste
4. indici e criteri di validazione
5. estrazione del contributo variabile "lock" alla riduzione delle concentrazioni



Corso 08
========================================================

1. analisi, grafici, reportistica ed integrazione con github
