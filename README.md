一个普通的如果执行的时间太长会阻塞调度器。

```
rebar compile -> 编译
erl -pa ebin -> 进入shell.
```


## 同步和异步nif的时间对调度器的阻塞

```
直接阻塞调度器
elua_block:sync().
```


```
调度器安全.
elua_block:async().
```


## 2ms nif的并发测试

可以在lua/block_400ms.lua中修改sleep的第1个参数作为处理ms.

-> {Max,Min,Avg}

```
30000个同步2ms的调用, 期间会出现卡顿
elua_block:r400ms_s(30000).

finis 26 {10,0,0.8410666666666666}
```

```
异步版无卡顿，最后完成时间: 20709 ms
elua_block:r400ms_a(30000).

finis 22 {20,0,0.7078}
```
