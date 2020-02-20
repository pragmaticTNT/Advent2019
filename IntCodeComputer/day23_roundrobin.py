from intcode_computer import IntcodeComputer

from typing import List
from collections import deque
import sys, fileinput, time

class NIC:
    def __init__(self, pid: int, msg_buffers):
        self._pid = pid
        self._msg_buffers = msg_buffers

        # For getting inputs
        self._first_input = True
        self._Y: Optional[int] = None

        # For sending outputs
        self._partial_msg = []

    def get(self) -> int:
        # Give the cpu its pid.
        if self._first_input:
            print(f"Set pid: {self._pid}")
            self._first_input = False
            return self._pid

        # Yield the second half of a half-eaten msg.
        if self._Y is not None:
            Y = self._Y
            self._Y = None
            print(f"    {self._pid} Y", Y)
            return Y

        if self._msg_buffers[self._pid]:
            pair = self._msg_buffers[self._pid].popleft()
            print(f"get {self._pid}", pair)
        else:
            return -1

        # Save the rest for later.
        X, Y = pair
        self._Y = Y
        print(f"    {self._pid} X", X)
        return X

    def put(self, val: int) -> None:
        self._partial_msg.append(val)

        if len(self._partial_msg) == 3:
            print(f'put {self._pid} {self._partial_msg}')
            target_pid, X, Y = self._partial_msg
            if target_pid == 255:
                print(Y)
                ##sys.exit()
            assert 0 <= target_pid < 50
            self._msg_buffers[target_pid].append((X, Y))
            self._parial_msg = []

def main():
    input_str = next(iter(fileinput.input()))
    program = list(map(int, input_str.split(",")))

    num_cpus = 50

    # queues[i] is a message buffer for the inputs to cpu i
    queues = [deque() for i in range(num_cpus)]
    nics = [NIC(i, queues) for i in range(num_cpus)]
    ipc = [IntcodeComputer(program) for i in range(num_cpus)]
    for i, pc in enumerate(ipc):
        pc.run(get_input_fn = nics[i].get, send_output_fn = nics[i].put, continuous = 0)

    # Function to be run by each thread (one thread per cpu).
    #def run_cpu(pid: int) -> None:
    #    IntcodeComputer(program).run(
    #            get_input_fn=nics[pid].get,
    #            send_output_fn=nics[pid].put,
    #            continuous = 0)

    counter = 0
    while True:
        val = ipc[counter].run(continuous = 0)
        if val >= 0:
            ipc[counter].run(continuous = 0)
        counter = (counter + 1) & num_cpus


if __name__ == "__main__":
    main()
