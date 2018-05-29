from flask import Flask, render_template, flash, request, jsonify, send_from_directory, redirect, url_for
import subprocess
import sys
import os
import time
from multiprocessing import Process

app = Flask(__name__, static_url_path='')
app.debug=True

projects = {}
currentProject = {}
array = []

def workerFunction():
    while True:
        time.sleep(10)
        print ("splip pasa")

@app.route('/js/<path:path>')
def send_js(path):
    return send_from_directory('templates/js', path)

@app.route('/images/<path:path>')
def send_images(path):
    return send_from_directory('templates/images', path)

@app.route('/css/<path:path>')
def send_css(path):
    return send_from_directory('templates/css', path)

@app.route('/generated/<path:path>')
def send_generated(path):
    return send_from_directory('generated', path)

@app.route("/")
def hello_world():
    return render_template('pages/index.html')

@app.route('/vaccination')
def vaccination():
    return render_template('pages/vaccination.html')

@app.route('/screening')
def screening():
    return render_template('pages/screening.html')

@app.route('/results')
def results():
    return render_template('pages/chart.html')

@app.route('/api/projects', methods=['GET', 'POST'])
def form_example():
    if request.method == 'POST':
        content = request.get_json()
        if content['projectName'] in projects:
            return 'Project already exists'
        createProject(content)
        return 'Project created'

    return jsonify(results = projects)

@app.route('/api/test')
def test_json():
    list = [
            {'a': 1, 'b': 2},
            {'a': 5, 'b': 10, 'c':555, 'd':555}
           ]
    return jsonify(results = list)

@app.route('/api/projects2', methods=['GET', 'POST'])
def projectNamePost():
    if request.method == 'POST':
        projectName = request.form['projectName']
        if projectName in projects:
            return 'Project already exists'
        createProject(projectName)
    return render_template('pages/vaccination.html')

@app.route('/api/vaccination', methods=['GET', 'POST'])
def vaccinationPost():
    if request.method == 'POST':
        vaccination = {}
        vaccination['malCheck'] = request.form.get('malCheck')
        vaccination['femCheck'] = request.form.get('femCheck')
        vaccination['femAgeRange'] = request.form.get('femAgeRange')
        vaccination['femYears'] = request.form.get('femYears')
        vaccination['femPercentage'] = request.form.get('femPercentage')
        vaccination['malAgeRange'] = request.form.get('malAgeRange')
        vaccination['malYears'] = request.form.get('malYears')
        vaccination['malPercentage'] = request.form.get('malPercentage')
        currentProject['vaccination'] = vaccination

    return render_template('pages/screening.html')

@app.route('/api/screening', methods=['GET', 'POST'])
def screeningPost():
    if request.method == 'POST':
        screening = {}
        screening['primCheck'] = request.form.get('primCheck')
        screening['triCheck'] = request.form.get('triCheck')
        screening['folCheck'] = request.form.get('folCheck')

        screening['selPrimTest'] = request.form.get('selPrimTest')
        screening['agePrimTest'] = request.form.get('agePrimTest')
        screening['freqPrimTest'] = request.form.get('freqPrimTest')

        screening['selTriageTest'] = request.form.get('selTriageTest')
        screening['ageTriageTest'] = request.form.get('ageTriageTest')
        screening['freqTriageTest'] = request.form.get('freqTriageTest')

        screening['selFollTest'] = request.form.get('selFollTest')
        screening['timeFollTest'] = request.form.get('timeFollTest')

        currentProject['screening'] = screening

        print ("Proyecto actual")
        print ( currentProject )

        vacRangeFemaleIni = currentProject['vaccination']['femAgeRange'].split(";")[0]
        vacRangeFemaleEnd = currentProject['vaccination']['femAgeRange'].split(";")[1]
        vacYearsFemaleIni = currentProject['vaccination']['femYears'].split(";")[0]
        vacYearsFemaleEnd = currentProject['vaccination']['femYears'].split(";")[1]
        vacPercentageFemale = currentProject['vaccination']['femPercentage']

        vacRangeMaleIni = currentProject['vaccination']['malAgeRange'].split(";")[0]
        vacRangeMaleEnd = currentProject['vaccination']['malAgeRange'].split(";")[1]
        vacYearsMaleIni = currentProject['vaccination']['malYears'].split(";")[0]
        vacYearsMaleEnd = currentProject['vaccination']['malYears'].split(";")[1]
        vacPercentageMale = currentProject['vaccination']['malPercentage']

        vacSex = "2"

        primaryTest = screening['selPrimTest']
        iniPrimaryTest = screening['agePrimTest'].split(";")[0]
        maxPrimaryTest = screening['agePrimTest'].split(";")[1]
        stepPrimaryTest = screening['freqPrimTest']

        triageTest = screening['selTriageTest']
        iniTriage = screening['ageTriageTest'].split(";")[0]
        maxTriage = screening['ageTriageTest'].split(";")[1]
        stepTriage = screening['freqTriageTest']

        followUp = screening['selFollTest']
        timeFollowUp = screening['timeFollTest']

        fileName = "generated/"+currentProject['name']+".csv"

        commands = ["Rscript", "funciones.R", vacRangeFemaleIni, vacRangeFemaleEnd, vacRangeMaleIni, vacRangeMaleEnd, vacYearsFemaleIni, vacYearsFemaleEnd, vacYearsMaleIni, vacYearsMaleEnd, vacSex, vacPercentageFemale, vacPercentageMale, primaryTest, iniPrimaryTest, maxPrimaryTest, stepPrimaryTest, triageTest, iniTriage, maxTriage, stepTriage, followUp, timeFollowUp, fileName]
        print (commands)
        #runCommand(commands)
        return redirect(url_for('results', fileName=currentProject['name']+".csv"))

def createProject(p):
    print (array)
    currentProject['name'] = p
    path = p

def runCommand (command):
    result= subprocess.Popen(command, stdout=subprocess.PIPE, universal_newlines=True)
    print(result.stdout)
    return result.stdout
#if __name__ == '__main__':    
    #worker = Process(target=workerFunction)
    #worker.start()
app.run(debug=True)


