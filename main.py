from flask import Flask, render_template, flash, request, jsonify, send_from_directory
import subprocess
import sys
import os

app = Flask(__name__, static_url_path='')
app.run(debug=True)

projects = {}
currentProject = {}

@app.route('/js/<path:path>')
def send_js(path):
    return send_from_directory('templates/js', path)

@app.route('/images/<path:path>')
def send_images(path):
    return send_from_directory('templates/images', path)

@app.route('/css/<path:path>')
def send_css(path):
    return send_from_directory('templates/css', path)

@app.route('/')
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
    return render_template('pages/results.html')

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

        if request.form.get('malCheck') is None:
            vaccination['malAgeRange'] = 'NA'
            vaccination['malYears'] = 'NA'
            vaccination['malPercentage'] = 'NA'

        if request.form.get('femCheck') is None:
            vaccination['femAgeRange'] = 'NA'
            vaccination['femYears'] = 'NA'
            vaccination['femPercentage'] = 'NA'
        currentProject['vaccination'] = vaccination
        print (currentProject)

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
        print (currentProject)
        return jsonify(results = currentProject)

def createProject(p):
    currentProject['name'] = p
    path = p
    if not os.path.exists(path):
        os.makedirs(path)
        runCommand(["ls"])

def runCommand (command):
    result= subprocess.run(command, stdout=subprocess.PIPE, universal_newlines=True)
    print(result.stdout)
    return result.stdout