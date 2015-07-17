import SimPy.Simulation as Simulation
import random


class SynchronizedMuUpdater(Simulation.Process):

    def __init__(self, server, intervalParam, serviceTime, rateChangeFactor,
                 serviceRateMonitor):
        self.server = server
        self.intervalParam = intervalParam
        self.serviceTime = serviceTime
        self.rateChangeFactor = rateChangeFactor
        self.serviceRateMonitor = serviceRateMonitor
        Simulation.Process.__init__(self, name='MuUpdater')

    def run(self):
        while(1):
            yield Simulation.hold, self,

            if (self.server.serviceTime == self.serviceTime):
                rate = 1/float(self.serviceTime)
                rate = self.rateChangeFactor * rate
                self.server.serviceTime = 1/float(rate)
            else:
                self.server.serviceTime = self.serviceTime
            serviceRate = 1/float(self.server.serviceTime)
            self.serviceRateMonitor.observe("%s %s" % (self.server.id,
                                                       serviceRate))
            # print Simulation.now(), self.server.id, self.server.serviceTime
            yield Simulation.hold, self,  self.intervalParam
