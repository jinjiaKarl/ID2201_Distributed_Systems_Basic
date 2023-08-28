import http from 'k6/http';
import { check } from 'k6';


export const options = {
    duration: "10s",
    vus: 5,
    // metrics
    // 1. avg reqs per second
    // 2. median, 95%, 99% reqs duration
    summaryTrendStats: ['avg', 'min', 'med', 'max', 'p(95)', 'p(99)', 'p(99.99)', 'count'],
}

// k6 run k6_main.js > benchmark_main.txt
export default function () {
    const url = 'http://127.0.0.1:8081/foo'
    const res = http.get(url)
}