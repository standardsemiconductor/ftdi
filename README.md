# ftdi
[![Haskell CI](https://github.com/standardsemiconductor/ftdi/workflows/Haskell%20CI/badge.svg?branch=master)](https://github.com/standardsemiconductor/ftdi/actions/workflows/haskell.yml)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]

This library enables you to communicate with FTDI devices. It is implemented as a lightweight wrapper around the [usb](https://hackage.haskell.org/package/usb) library.

See [bindings-libusb](https://hackage.haskell.org/package/bindings-libusb) for instructions to install [libusb](https://libusb.info).

## References
* [FTDI Website](https://ftdichip.com/)
* [Application Note AN_108](https://www.ftdichip.com/Support/Documents/AppNotes/AN_108_Command_Processor_for_MPSSE_and_MCU_Host_Bus_Emulation_Modes.pdf): Command Processor for MPSSE and MCU Host Bus Emulation Modes

[hackage]:            <https://hackage.haskell.org/package/ftdi>
[hackage-badge]:      <https://img.shields.io/hackage/v/ftdi.svg?color=success>
[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/ftdi.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=ftdi>