# opslang
syntax definition and sample implementation of ops file language

## 基本構文

```
set DATETIME_ORIGIN=time!(2020-01-01T00:00:00Z) # 処理系のパラメータ設定

let ok = "OK"                       # string型
let t = time!(2024-01-01T00:00:00Z) # time型
let d = 1s                          # duration型
let ti = 2000                       # 数値型

.                                   # breakpoint機能
assert 2 == 2                       # WINGS: check_value
assert_eq 2, 2
assert_approx_eq 1.0, 1.0, 0.001    # WINGS: --tolerance 0.001
print 2                             # WINGS: get
wait 5s                             # WINGS: wait_sec
wait 1 == 1 && 2 <= 2               # WINGS: wait_until
wait 0 == 1 || 5s                   # タイムアウトつき

print tlmid!(MOBC.HK)               # telemetry id 参照
print $MOBC.HK.XX.YY
let hk = $MOBC.HK                   # テレメトリ集合型
print hk.OBC.XX.YY                  # テレメトリ集合型は構造体のように参照できる

@RT.MOBC NOP                                              # Time Indicator をとらないコマンド
@TL.MOBC 20: NOP                                          # ステップ単位のTime Indicatorをとるコマンド
@BL.MOBC 30: NOP
@UTL.MOBC time!(2024-01-01T00:00:00Z): NOP                # 時刻をTime Indicatorとしてとるコマンド
@TL.MOBC 20: @@RT.AOBC NOP                                # 最終的な実行コンポーネントが異なるコマンド (WINGSのMOBC_TL.AOBC_RTに相当)
```

## 糖衣構文

```
@RT.MOBC delay=0.5s { # comment
    # この中はデフォルトで @MOBC
    # 一行実行するごとにdelayだけwaitを挟む
    NOP
    NOP
} # comment

#    MOBC_TL.Cmd_NOP 10
#    wait_sec 0.5
#    MOBC_TL.Cmd_NOP 20
#    wait_sec 0.5
#    MOBC_TL.AOBC_RT.Cmd_NOP 30
#    wait_sec 0.5
#    MOBC_TL.AOBC_RT.Cmd_NOP 40
#    wait_sec 0.5
@TL.MOBC delay=0.5s {
    10: NOP
    20: NOP
    30: @@RT.AOBC NOP
    40: @@RT.AOBC NOP
}

@TL.MOBC delay=0.5s {
    10: NOP
    @RT.MOBC NOP #このブロック内はデフォルトでTL.MOBCだが、この行はRT.MOBC
}

```

## 検討中

```
# after!で直前の時間指定からの相対時間を指定できる
@TL.MOBC delay=0.5s {
    10: NOP                              # TI=10
    assert_eq last_ti!(), 10
    last_ti!() + 10: NOP                 # TI=20
    assert_eq last_ti!(), 20
    last_ti!() + 10: @@AOBC NOP               # TI=30
    assert_eq last_ti!(), 30
}
```
