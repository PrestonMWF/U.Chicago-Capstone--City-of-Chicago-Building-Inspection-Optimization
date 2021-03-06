import numpy as np
import math
import random
import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt

#####################################


def calculateObj(route, distances, dff):
    if len(route) == 0:
        return -99999999

    objVal = 0
    totDist = 0

    for i in range(1, len(route)):
        objVal = objVal + 10*dff[route[i]][2] - distances[route[i - 1]][route[i]]
        totDist = totDist + distances[route[i - 1]][route[i]]
    totDist = totDist + 0.525 * len(route)
    if totDist > 8.4:
        objVal = -9999
    else:
        objVal = objVal + 0.5 * len(route)
    return objVal


def calculateTour(route, distances):
    objVal = 0

    for i in range(1, len(route)):
        objVal = objVal + distances[route[i - 1]][route[i]]

    return objVal


def twoOpt(route, distances):
    se = (0, 0)
    xx = 0
    while (True):
        xx = xx + 1
        temp_route = list(route)
        old_route = list(route)
        route_distance = -999999999
        for i in range(1, len(route) - 2):
            for j in range(i + 1, len(route) - 1):
                new_route = route[:i] + list(reversed(route[i:j + 1])) + route[j + 1:]
                diff_distance = distances[route[i - 1]][route[i]] + distances[route[j]][route[j + 1]]
                diff_distance = diff_distance - distances[new_route[i - 1]][new_route[i]] - distances[new_route[j]][
                    new_route[j + 1]]
                if diff_distance > route_distance:
                    temp_route = list(new_route)
                    route_distance = diff_distance
                    se = (i, j)
        if route_distance > 0.01:
            route = list(temp_route)
        else:
            break
    return route


def threeOptSwap(route, i, j, k, distances):
    bestRoute = list(route)
    best_diff = 0

    a = i
    b = j + 1
    c = k + 2

    nRoute = route[:a] + list(reversed(route[a:b])) + list(reversed(route[b:c])) + route[c:]
    diff = distances[route[a - 1]][route[a]] + distances[route[b - 1]][route[b]] + distances[route[c - 1]][route[c]]
    diff = diff - distances[route[a - 1]][route[b - 1]] - distances[route[a]][route[c - 1]] - distances[route[b]][
        route[c]]
    if diff > best_diff:
        best_diff = diff
        bestRoute = list(nRoute)

    nRoute = route[:a] + route[b:c] + route[a:b] + route[c:]
    diff = distances[route[a - 1]][route[a]] + distances[route[b - 1]][route[b]] + distances[route[c - 1]][route[c]]
    diff = diff - distances[route[a - 1]][route[b]] - distances[route[c - 1]][route[a]] - distances[route[b - 1]][
        route[c]]
    if diff > best_diff:
        best_diff = diff
        bestRoute = list(nRoute)

    nRoute = route[:a] + route[b:c] + list(reversed(route[a:b])) + route[c:]
    diff = distances[route[a - 1]][route[a]] + distances[route[b - 1]][route[b]] + distances[route[c - 1]][route[c]]
    diff = diff - distances[route[a - 1]][route[b]] - distances[route[c - 1]][route[b - 1]] - distances[route[a]][
        route[c]]
    if diff > best_diff:
        best_diff = diff
        bestRoute = list(nRoute)

    nRoute = route[:a] + list(reversed(route[b:c])) + route[a:b] + route[c:]
    diff = distances[route[a - 1]][route[a]] + distances[route[b - 1]][route[b]] + distances[route[c - 1]][route[c]]
    diff = diff - distances[route[a - 1]][route[c - 1]] - distances[route[b]][route[a]] - distances[route[b - 1]][
        route[c]]
    if diff > best_diff:
        best_diff = diff
        bestRoute = list(nRoute)

    return bestRoute, best_diff


def threeOpt(route, distances):
    xx = 0
    while (True):
        xx += 1
        temp_route = list(route)
        old_route = list(route)
        best_diff = 0.01
        brk = False
        li = list(range(1, len(route) - 2))
        random.shuffle(li)
        for i in li:
            lj = list(range(i, len(route) - 2))
            random.shuffle(lj)
            for j in lj:
                lk = list(range(j, len(route) - 2))
                random.shuffle(lk)
                for k in lk:
                    new_route, new_diff = threeOptSwap(route, i, j, k, distances)
                    if new_diff > best_diff:
                        temp_route = list(new_route)
                        best_diff = new_diff
                        brk = True
                        break
                if brk:
                    break
            if brk:
                break
        if not brk:
            break
        if best_diff > 0.01:
            route = list(temp_route)
        else:
            break
    return route


def initialization(distances, dff, prof, N):
    ''' Construction Heuristic '''
    best_objs = []
    best_routes = []
    for i in [int(N / 4)]:
        local_obj = -99999999
        local_route = []
        for t in range(20):
            route = [1, 1]
            for j in range(i):
                min_obj = 99999999
                k = random.randint(0, len(route) - 2)
                temp_route = list(route)
                for lab in range(1, N + 1):
                    if lab not in route:
                        new_route = route[:k + 1] + [lab] + route[k + 1:]
                        diff_obj = (distances[route[k]][lab] + distances[lab][route[k + 1]] - distances[route[k]][
                            route[k + 1]]) / prof[lab]
                        if diff_obj < min_obj:
                            temp_route = list(new_route)
                            min_obj = diff_obj
                route = list(temp_route)
            temp_route = twoOpt(route, distances)
            temp_obj = calculateObj(temp_route, distances, dff)
            if temp_obj > local_obj:
                local_obj = temp_obj
                local_route = list(temp_route)

        best_routes.append(local_route)
        best_objs.append(local_obj)

    route = list(best_routes[0])
    rat = 0
    for i in range(len(best_routes)):
        if best_objs[i] / len(best_routes[i]) > rat:
            rat = best_objs[i] / len(best_routes[i])
            route = list(best_routes[i])

    return route


def dispersionIndex(cluster, distances):
    if len(cluster) == 1:
        return 0
    else:
        sm = 0
        for c1 in cluster:
            for c2 in cluster:
                sm = sm + distances[c1][c2]
        return sm / (len(cluster) * (len(cluster) - 1))


def proximityMeasure(cluster1, cluster2, distances):
    sm = 0
    for c1 in cluster1:
        for c2 in cluster2:
            sm = sm + distances[c1][c2]

    return (2 / (len(cluster1) * len(cluster2))) * sm - dispersionIndex(cluster1, distances) - dispersionIndex(cluster2, distances)


def insertionCandidates(N, distances):
    candidates = []
    rList = [1, int(N / 2), int(2 * N / 3), int(3 * N / 4), int(4 * N / 5), int(5 * N / 6), int(6 * N / 7),
             int(7 * N / 8), int(8 * N / 9), int(9 * N / 10)]

    Pr = []
    Pr = [[x] for x in range(2, N + 1)]
    candidates.append(list(Pr))

    for r in range(2, N):
        minProx = 99999999
        minProxInd = []
        for i in range(len(Pr)):
            for j in range(i + 1, len(Pr)):
                pM = proximityMeasure(Pr[i], Pr[j], distances)
                if pM < minProx:
                    minProx = pM
                    minProxInd = [i, j]
        Pr.append(Pr[minProxInd[0]] + Pr[minProxInd[1]])
        del (Pr[minProxInd[1]])
        del (Pr[minProxInd[0]])

        if r in rList:
            candidates.append(list(Pr))

    return candidates


def deletionCandidates(route, distances):
    candidates = []
    edges = []

    K = random.randint(2, int(max(4, len(route)) / 2))

    for i in range(len(route) - 1):
        edges.append([distances[route[i]][route[i + 1]], i, i + 1])

    edges = list(reversed(sorted(edges)))[:K]
    edges.sort(key=lambda x: x[1])

    for i in range(K - 1):
        tempList = []
        for j in range(edges[i][2], edges[i + 1][1] + 1):
            tempList.append(route[j])

        candidates.append(tempList)

    return candidates


def findBestInsertionCandidate(route, tabuList, insCandidates, dff):
    bestInsCandidate = []
    bestInsObj = -99999999

    for iC in insCandidates:
        profitSum = 0
        gCenter = [0, 0]
        for c in iC:
            if c not in route and c not in tabuList:
                gCenter[0] = gCenter[0] + dff[c][0] / len(iC)
                gCenter[1] = gCenter[1] + dff[c][1] / len(iC)
                profitSum = profitSum + dff[c][2]

        minDist = 99999999
        for j in range(len(route) - 1):
            distAdd1 = calculateDist(dff[route[j]][0], dff[route[j]][1], gCenter[0], gCenter[1])
            distAdd2 = calculateDist(gCenter[0], gCenter[1], dff[route[j + 1]][0], dff[route[j + 1]][1])
            distRem = calculateDist(dff[route[j]][0], dff[route[j]][1], dff[route[j + 1]][0], dff[route[j + 1]][1])

            dist = distAdd1 + distAdd2 - distRem
            if dist < minDist:
                minDist = dist

        if profitSum / minDist > bestInsObj:
            bestInsObj = profitSum / minDist
            bestInsCandidate = list(iC)

    return bestInsCandidate


def calculateDist(x1, y1, x2, y2):
    return (np.abs(math.pow(x1 - x2, 1)) + np.abs(math.pow(y1 - y2, 1)))

########################################################################

def TabuSearch(dataframe, start_lon, start_lat):

    # load data
    df_full = pd.DataFrame(dataframe)
    df = df_full[['longitude.x', 'latitude.x', 'logistic_probs', 'status', 'sr_number', 'created_date', 'street_address']]
    df = df[df.status != 'completed']
    df.columns = ['x', 'y', 'score', 'status', 'sr_number', 'created_date', 'street_address']

    start = pd.DataFrame([[start_lon, start_lat, 0, 'start', 'NA', 'NA', 'NA']],
                         columns=['x', 'y', 'score', 'status', 'sr_number', 'created_date', 'street_address'])

    # filter to closest +- 5 miles
    df = df[(df.y <= start_lat + 0.0375) &
            (df.y >= start_lat - 0.0375) &
            (df.x <= start_lon + 0.05) &
            (df.x >= start_lon - 0.05)]

    df = start.append(df, ignore_index=True)

    N = len(df) - 1

    distances = [-1]
    prof = [-1]

    for lab, row in df.iterrows():
        tempDist = [-1]
        prof.append(row['score'])
        for lab2, row2 in df.iterrows():
            dist = np.abs(row['x'] - row2['x']) + np.abs(row['y'] - row2['y'])
            tempDist.append(dist)
        distances.append(tempDist)

    # dff holds the main data as given from the xls
    # Started the indices from 1
    dff = [[0, 0, 0]]
    for lab, row in df.iterrows():
        dff.append([row['x'], row['y'], row['score']])

    # Iteration Count
    ITER = 10000

    # Create the initial route
    route = initialization(distances, dff, prof, N)

    # Determine all possible insertion partitions
    insCandidatesAll = insertionCandidates(N, distances)
    tabuList = {}
    solutionIndex = [0]

    bestRoute = list(route)
    bestObj = calculateObj(bestRoute, distances, dff)

    # Start tabu search
    for i in range(ITER):
        # Choose one insertion partition randomly
        insCandidates = list(insCandidatesAll[random.randint(0, len(insCandidatesAll) - 1)])

        # Determine deletion candidates
        if len(route) < 3:
            delCandidates = []
        else:
            delCandidates = deletionCandidates(route, distances)

        candidateRoute = []
        tabuAddition = []

        # Find best insertion candidate from the selected partition
        bestInsCandidate = findBestInsertionCandidate(route, tabuList, insCandidates, dff)

        # Calculate the gain of inserting the insertion candidate to the route
        insertedRoute = list(route)
        profitSum = 0
        distSum = 0
        random.shuffle(bestInsCandidate)
        for c in bestInsCandidate:
            if c not in insertedRoute and c not in tabuList:
                profitSum = profitSum + dff[c][2]
                minDist = 99999999
                temp_route = list(insertedRoute)
                for j in range(len(insertedRoute) - 1):
                    new_route = insertedRoute[:j + 1] + [c] + insertedRoute[j + 1:]
                    diffDist = distances[insertedRoute[j]][c] + distances[c][insertedRoute[j + 1]] - \
                               distances[insertedRoute[j]][insertedRoute[j + 1]]
                    if diffDist < minDist:
                        temp_route = list(new_route)
                        minDist = diffDist
                insertedRoute = list(temp_route)
                distSum = distSum + minDist
        if distSum == 0:
            distSum = 99999999
        insertedObj = calculateObj(insertedRoute, distances, dff)

        # Choose the best deletion candidate from the selected ones, then calculate its gain
        deletedRoute = list(route)
        maxDeletedObj = -99999999
        for dC in delCandidates:
            tempRoute = list(route)
            profitSum = 0
            distSum = 0
            for c in dC:
                if c in tempRoute:
                    cPrev = tempRoute[tempRoute.index(c) - 1]
                    cNext = tempRoute[tempRoute.index(c) + 1]

                    profitSum = profitSum + dff[c][2]
                    distSum = distances[cPrev][c] + distances[c][cNext] - distances[cPrev][cNext]
                    tempRoute.remove(c)
            if profitSum != 0 and distSum / profitSum > maxDeletedObj:
                maxDeletedObj = distSum / profitSum
                deletedRoute = list(tempRoute)
                tabuAddition = list(dC)
        deletedObj = calculateObj(deletedRoute, distances, dff)

        # Compare the insertion and deletion gains, and apply the better one
        if insertedObj > deletedObj:
            candidateRoute = list(insertedRoute)
            chosen = ['I', len(insertedRoute) - len(route)]
        else:
            candidateRoute = list(deletedRoute)
            chosen = ['D', len(route) - len(deletedRoute)]

        # Update the tabu list
        for key, value in list(tabuList.items()):
            tabuList[key] = tabuList[key] - 1
            if tabuList[key] == 0:
                del (tabuList[key])

        # If deletion action is performed then add the chosen deletion candidates to the tabu list.
        if chosen[0] == 'D':
            for tA in tabuAddition:
                if tA in route:
                    tabuList[tA] = random.randint(5, 25)

        route = list(candidateRoute)

        # Improve the route
        if i % 5 == 0:
            route = twoOpt(route, distances)

        # Best solution update
        if calculateObj(route, distances, dff) > bestObj:
            solutionIndex.append(i)
            route = threeOpt(route, distances)
            bestRoute = list(route)
            bestObj = calculateObj(route, distances, dff)

        # Shuffle to Reset
        if i - solutionIndex[-1] >= 500:
            tabuList.clear()
            tempRoute = bestRoute[1:-1]
            random.shuffle(tempRoute)
            tempRoute = [1] + tempRoute + [1]
            route = list(tempRoute)
            solutionIndex.append(i)

    finalRoute = [x - 1 for x in bestRoute]

    return df.iloc[finalRoute, ]
