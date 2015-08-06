一个普通的如果执行的时间太长会阻塞调度器。

```
rebar compile -> 编译
erl -pa ebin -> 进入shell.
```

## 同步和异步nif的时间对调度器的阻塞

执行cpu数量个进程，每个进程call到lua sleep 2s.

```
直接阻塞调度器，block的时候无法输出tick.
elua_block:sync().
```

```
调度器安全，一切正常。
elua_block:async().
```

## 2ms nif的并发测试

同步或者异步执行3w个lua进程。

可以在lua/block_400ms.lua中修改sleep的第1个参数作为处理ms.

-> finis 总时间 {Max,Min,Avg}

```
30000个同步2ms的调用, 期间会出现卡顿，CPU无法跑满2个核心以上
elua_block:r400ms_s(30000).
case 1 finis 31 {30022.732,5501.662,18074.107647166762}
case 2 finis 27 {26849.639,2370.878,15011.300719133396}
```

```
30000个同步2ms的调用, 无卡顿，CPU可跑到250%.
elua_block:r400ms_a(30000).
case 1 finis 20 {19474.931,52.119,11403.61188513333}
case 2 finis 19 {18437.785,41.009,10398.796304300015}
```
