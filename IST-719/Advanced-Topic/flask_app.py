#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  6 08:06:28 2022

@author: venkatasharatsripada
"""

# Flask application for sales data

from flask import Flask, flash, redirect, render_template, request, session, abort, send_from_directory, send_file, jsonify
import pandas as pd
import json

#1. Declare application
application = Flask(__name__)

#2. Declare data stores
class DataStore():
    WineType = None
    Year = None
    Sold = None

data=DataStore()
    
@application.route("/main", methods=["GET","POST"])


#3. Define main code
@application.route("/", methods=["GET","POST"])
def homepage():
    Wine_Type = request.form.get('Wine_Type', 'white')
    Year = request.form.get('Year', 2010)
    data.WineType = Wine_Type
    data.Year = Year
    df = pd.read_csv('sales.csv')

    # choose columns to keep, in the desired nested json hierarchical order
    df = df[df.type == Wine_Type]
    df = df[df.year == int(Year)]
    print(df.head())


    df = df[["rep.region", "sales.rep", "units.sold", "year"]]

    # order in the groupby here matters, it determines the json nesting
    # the groupby call makes a pandas series by grouping 'the_parent' and 'the_child', while summing the numerical column 'child_size'
    df1 = df.groupby(['rep.region', 'sales.rep', 'year'])['units.sold'].sum()
    df1 = df1.reset_index()

    # start a new flare.json document
    flare = dict()
    d = {"name": "flare", "children": []}
    for row in df1.values:
        region = row[0]
        rep = row[1]
        year = row[2]
        sold = row[3]
        # make a list of keys
        keys_list = []
        for item in d['children']:
            keys_list.append(item['name'])
        # if 'the_parent' is NOT a key in the flare.json yet, append it
        if region not in keys_list:
            d['children'].append({"name": region, "children": 
                                  [{"name": rep,
                                    "size": sold}]})
        # if 'the_parent' IS a key in the flare.json, add a new child to it
        else:
            d['children'][keys_list.index(region)]['children'].append({"name": rep, 
                                                                     "size": sold})
    flare = d
    e = json.dumps(flare)
    data.Sold = json.loads(e)
    Sold = data.Sold

    return render_template("index.html", Wine=Wine_Type, Sold=Sold)

@application.route("/get-data", methods=["GET","POST"])
def returnData():
    # Sample crops:  
    #   f = {'name': 'flare', 'children': [{'name': 'Vegetables', 'children': [{'name': 'Vegetables, Other', 'size': 4668}]}]}
    # Sample sales:
    #   f = {'name': 'flare', 'children': [{'name': 'Central', 'children': [{'name': 'Dannette Saltsman', 'size': 676}]}]}
    f = data.Sold
    print(f)
    return jsonify(f)


if __name__ == "__main__":
    application.run(debug=True)
