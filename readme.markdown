Uthernet II Snooper CDA

This is a Classic Desk Accessory to peek and poke at the Uthernet II/Wiznet w5100 registers.

Slot 3 is assumed.

Built with the SNASM assembler and [snlink](https://github.com/ksherlock/snlink), sorry.


Keys:

* C - Common registers
* 0 - Socket 0
* 1 - Socket 1
* 2 - Socket 2
* 3 - Socket 3
* R - Receive buffer
* T - Transmit buffer
* Left arrow, right arrow - switch between pages
* Up arrow, down arrow - scroll through memory (RX/TX only)
* : - write memory.
* Esc - quit
* Q - quit

Writing memory:

Format is `address : xx [xx ...]` where xx is a hexadecimal byte and address is 8-bit or 16-bit hexadecimal. spaces are optional.

For the socket registers, the page high-byte (`$00`, `$04`, `$05`, etc) is pre-loaded so an 8-bit address
will work as expected.  The display will be reloaded after a write but keep in mind some registers are not
visibly changed until a command is entered (eg, tx write doesn't update until a send command).

Be careful writing the MR register ($00) :-)

