(ns atividade.atividade-notas)

;; Função para obter conceito da nota
(defn obter-conceito [nota]
  (cond
    (>= nota 90) "A (Excelente)"
    (>= nota 80) "B (Bom)"
    (>= nota 70) "C (Satisfatório)"
    (>= nota 60) "D (Precisa melhorar)"
    :else "F (Reprovado)"))

;; Função para ler número com validação
(defn ler-numero [prompt]
  (println prompt)
  (loop []
    (let [entrada (read-line)]
      (if (re-matches #"\d+(\.\d+)?" entrada) ; Verifica se é um número válido
        (Double. entrada)
        (do
          (println "Entrada inválida, tente novamente.")
          (recur)))))

;; Função para ler quantidade de alunos com validação
(defn ler-quantidade-alunos []
  (loop []
    (println "Quantos alunos na turma?")
    (let [qtd-alunos (Integer. (read-line))]
      (if (<= qtd-alunos 0)
        (do
          (println "Número de alunos deve ser maior que zero.")
          (recur))
        qtd-alunos))))

;; Função para coletar alunos e suas notas
(defn coletar-alunos [qtd-alunos]
  (loop [i 1 alunos []]
    (if (> i qtd-alunos)
      alunos
      (do
        (println (str "Nome do aluno " i ":"))
        (let [nome (read-line)]
          (println "Nota:")
          (let [nota (ler-numero "Digite a nota do aluno:")
                conceito (obter-conceito nota)]
            (println (str nome " - Conceito: " conceito))
            (recur (inc i) (conj alunos {:nome nome :nota nota}))))))))

;; Funções para calcular média, contar aprovados e desempenho geral
(defn calcular-media [alunos]
  (/ (reduce + (map :nota alunos)) (count alunos)))

(defn contar-aprovados [alunos]
  (count (filter #(>= (:nota %) 60) alunos)))

(defn desempenho-geral [media]
  (cond
    (>= media 80) "Turma excelente!"
    (>= media 60) "Bom desempenho!"
    :else "É necessário melhorar!"))

;; Função para formatar a média
(defn formatar-media [media]
  (format "%.2f" media))

;; Função principal para rodar o programa
(defn -main []
  (let [qtd-alunos (ler-quantidade-alunos)
        alunos (coletar-alunos qtd-alunos)
        media (calcular-media alunos)
        aprovados (contar-aprovados alunos)]
    (println (str "Média da turma: " (formatar-media media)))
    (println (str "Aprovados: " aprovados))
    (println (str "Desempenho geral: " (desempenho-geral media)))))
