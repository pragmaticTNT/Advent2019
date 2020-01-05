from intcode_computer import IntcodeComputer

from typing import List
from queue import Queue
from threading import Thread
import sys, fileinput, queue, time

class NIC:
    def __init__(self, pid: int, msg_buffers: List[Queue]):
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

        try:
            pair = self._msg_buffers[self._pid].get_nowait()
            print(f"get {self._pid}", pair)
        except queue.Empty:
            #print('empty')
            time.sleep(0.01)
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
                sys.exit()
            assert 0 <= target_pid < 50
            self._msg_buffers[target_pid].put((X, Y))
            self._parial_msg = []

def main():
    input_str = next(iter(fileinput.input()))
    program = list(map(int, input_str.split(",")))

    num_cpus = 50

    # queues[i] is a message buffer for the inputs to cpu i
    queues = [Queue() for i in range(num_cpus)]
    nics = [NIC(i, queues) for i in range(num_cpus)]

    # Function to be run by each thread (one thread per cpu).
    def run_cpu(pid: int) -> None:
        IntcodeComputer(program).run(
                get_input_fn=nics[pid].get,
                send_output_fn=nics[pid].put)

    # Run all the threads concurrently.
    threads = []
    for i in range(num_cpus):
        t = Thread(target=run_cpu, args=(i,))
        t.start()
        threads.append(t)
    for t in threads:
        t.join()

if __name__ == "__main__":
    main()
