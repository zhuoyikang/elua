一个普通的如果执行的时间太长会阻塞调度器。

rebar compile -> 编译

erl -pa ebin

```
阻塞版: elua_block:sync().
异步版: elua_block:async().
```
