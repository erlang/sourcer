#!groovy

pipeline {
	agent any
	options {
		disableConcurrentBuilds()
		timestamps()
		skipDefaultCheckout()
		buildDiscarder(logRotator(numToKeepStr: '10'))
	}
	stages {
		stage('Checkout') {
			steps{
				retry(3) {
					timeout(time: 30, unit: 'SECONDS') {
						script {
							checkout()
						}
					}
				}
			}
		}

		stage('Compile') {
			steps{
				script {
					compile()
					analyze1()
				}
			}
		}

		stage('Test') {
			steps{
				script {
					test()
					analyze2()
				}
			}
		}

		stage('Server') {
			steps{
				script {
                    buildServer()
                    publishServer()
				}
			}
		}
	}
	//post {
		//always {
			//deleteDir()
		//}
	//}


}

///////////////////////////////////

def checkout() {
    deleteDir()
    checkout scm
    git_branch = env.BRANCH_NAME

    sh('git rev-parse HEAD > GIT_COMMIT')
    git_commit=readFile('GIT_COMMIT')
    short_commit=git_commit.take(6)

    currentBuild.setDescription("${git_branch} - ${short_commit}")
}

def compile() {
    sh "chmod u+x rebar3"
    sh "REBAR_COLOR='low' ~/erlide_tools/20.0/bin/escript ./rebar3 compile"
     sleep 2L
}

def test() {
    sh "chmod u+x rebar3"
    sh "REBAR_COLOR='low' ~/erlide_tools/20.0/bin/escript ./rebar3 eunit"
    sleep 2L
    sh "find . -name \"TEST-*.xml\" -exec xargs rename -v 's/\"//' {} \\;"
}

def analyze1() {
    step([$class: 'WarningsPublisher', canComputeNew: false, canResolveRelativePaths: true, canRunOnFailed: true,
        consoleParsers: [[parserName: 'Erlang Compiler (erlc)']],
        excludePattern: '', healthy: '', includePattern: '', messagesPattern: '', unHealthy: ''])
    step([$class: 'TasksPublisher', canComputeNew: false, excludePattern: '**/_build/**/*.*', healthy: '', high: 'FIXME,XXX', low: '', normal: 'TODO', pattern: '**/*.erl,**/*.hrl', unHealthy: ''])
}

def analyze2() {
    step([$class: 'AnalysisPublisher', canComputeNew: false, healthy: '', unHealthy: ''])
    step([$class: 'JUnitResultArchiver', allowEmptyResults: true, testResults: '**/TEST*.xml'])
	//step([$class: 'JacocoPublisher', exclusionPattern: '', sourcePattern: '**/src/'])
    // we need Cobertura...
    // publishHTML([
    //     allowMissing: false,
    //     alwaysLinkToLastBuild: false,
    //     keepAll: true,
    //     reportDir: '',
    //     reportFiles:
    //     '''common/_build/test/index.html
    //     ''',
    //     reportName: 'Coverage Report'
    //     ])
}

def buildServer() {
    sh "REBAR_COLOR='low' ~/erlide_tools/20.0/bin/escript ./rebar3 escriptize"
    step([$class: 'ArtifactArchiver', artifacts: "_build/default/bin/erlang_ls", fingerprint: true])
}

def publishServer() {
    
}