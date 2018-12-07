from . import flib


def xrotor():
    flib.rotor()


# class Rotor(object):
#     pass
#
#
# class XRotor(object):
#
#     def __init__(self):
#         super().__init__()
#         flib.init()
#         self._rotor = None
#
#     @property
#     def rotor(self):
#         return self._rotor
#
#     @rotor.setter
#     def rotor(self, new_rotor):
#         self._rotor = new_rotor
#         xrotor_lib.load(*new_rotor.items())
