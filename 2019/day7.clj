(ns day7-feedback
  (:require [clojure.string :as str]))

(defn parse-program [input-str]
  (mapv #(Integer/parseInt %) (str/split (str/trim input-str) #",")))

;; 虚拟机状态仅仅是一个不可变的 Map
(defn make-vm [memory inputs]
  {:mem    memory       ;; 内存 (Vector)
   :ip     0            ;; 指令指针
   :inputs inputs       ;; 输入缓冲区 (Vector)
   :status :running})   ;; 状态标记

;; 参数模式解析：立即模式(1)直接返回值，位置模式(0)查内存
(defn get-param [mem ptr mode]
  (let [val (get mem ptr)]
    (if (= mode 1) val (get mem val))))

(defn step [{:keys [mem ip inputs] :as vm}]
  (let [opcode-full (get mem ip)
        opcode      (mod opcode-full 100)
        modes       (quot opcode-full 100)
        ;; 辅助函数：获取第 N 个参数的值
        arg         (fn [n] (get-param mem (+ ip n 1) (mod (quot modes (int (Math/pow 10 n))) 10)))
        ;; 辅助函数：获取写入地址 (总是位置模式)
        dest        (fn [n] (get mem (+ ip n 1)))]

    (case opcode
      ;; 加法
      1 (assoc vm :mem (assoc mem (dest 2) (+ (arg 0) (arg 1)))
                  :ip  (+ ip 4))
      ;; 乘法
      2 (assoc vm :mem (assoc mem (dest 2) (* (arg 0) (arg 1)))
                  :ip  (+ ip 4))
      ;; 输入 (关键逻辑)
      3 (if (empty? inputs)
          (assoc vm :status :need-input) ;; 如果没输入，标记状态并暂停 IP 不动
          (assoc vm :mem (assoc mem (dest 0) (first inputs))
                    :inputs (vec (rest inputs))
                    :ip (+ ip 2)))
      ;; 输出
      4 (assoc vm :last-output (arg 0)   ;; 记录输出值
                  :status      :output   ;; 标记产生输出
                  :ip          (+ ip 2))
      ;; Jump-if-true
      5 (assoc vm :ip (if (zero? (arg 0)) (+ ip 3) (arg 1)))
      ;; Jump-if-false
      6 (assoc vm :ip (if (zero? (arg 0)) (arg 1) (+ ip 3)))
      ;; Less than
      7 (assoc vm :mem (assoc mem (dest 2) (if (< (arg 0) (arg 1)) 1 0))
                  :ip  (+ ip 4))
      ;; Equals
      8 (assoc vm :mem (assoc mem (dest 2) (if (= (arg 0) (arg 1)) 1 0))
                  :ip  (+ ip 4))
      ;; Halt
      99 (assoc vm :status :halted)

      (throw (ex-info "Unknown Opcode" {:opcode opcode :ip ip})))))

(defn run-until-event [vm]
  (loop [current-vm (assoc vm :status :running)] ;; 确保开始时是 running
    (if (not= (:status current-vm) :running)
      current-vm ;; 如果状态变了(output/wait/halt)，返回当前状态
      (recur (step current-vm)))))

(defn run-feedback-loop [program phases]
  ;; 1. 初始化：创建 5 个虚拟机，每个都预先填入对应的 Phase Setting
  (let [initial-amps (mapv #(make-vm program [%]) phases)]

    ;; 2. 循环调度
    (loop [amps   initial-amps ;; [VM-A, VM-B, VM-C, VM-D, VM-E]
           idx    0            ;; 当前索引 (0-4)
           signal 0]           ;; 初始信号 0

      (let [current-amp (get amps idx)
            ;; 关键：将信号注入当前放大器的输入缓冲区
            ;; update-in 是 Clojure 修改嵌套数据的神器
            ready-amp   (update current-amp :inputs conj signal)

            ;; 运行直到发生事件
            next-amp-state (run-until-event ready-amp)
            status         (:status next-amp-state)]

        (case status
          ;; 情况 A: 产生了输出
          :output
          (let [out-val (:last-output next-amp-state)
                ;; 更新放大器组状态，把跑完的新状态放回去
                new-amps (assoc amps idx (assoc next-amp-state :status :running))]
            ;; 传球：索引+1，输出值作为新信号
            (recur new-amps (mod (inc idx) 5) out-val))

          ;; 情况 B: 停机
          :halted
          (if (= idx 4)
            signal ;; 如果是放大器 E (idx 4) 停机，当前手中的信号就是最终答案
            (recur (assoc amps idx next-amp-state) (mod (inc idx) 5) signal))

          ;; 情况 C: 需要输入 (理论上在反馈回路中，我们会主动注入 signal，不应卡在这里)
          :need-input
          (throw (ex-info "Deadlock" {:amp idx :state next-amp-state})))))))

;; 简单的全排列生成器
(defn permutations [s]
  (if (empty? s)
    '(())
    (for [x s
          p (permutations (remove #{x} s))]
      (cons x p))))

(defn solve-part2 [input-str]
  (let [program (parse-program input-str)
        phases  [5 6 7 8 9]]
    (->> (permutations phases)
         (map #(run-feedback-loop program %))
         (apply max))))

;; --- 使用示例 ---
(def input "3,8,1001,8,10,8,105,1,0,0,21,46,67,76,101,118,199,280,361,442,99999,3,9,1002,9,4,9,1001,9,2,9,102,3,9,9,101,3,9,9,102,2,9,9,4,9,99,3,9,1001,9,3,9,102,2,9,9,1001,9,2,9,1002,9,3,9,4,9,99,3,9,101,3,9,9,4,9,99,3,9,1001,9,2,9,1002,9,5,9,101,5,9,9,1002,9,4,9,101,5,9,9,4,9,99,3,9,102,2,9,9,1001,9,5,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99")
(println (solve-part2 input))
