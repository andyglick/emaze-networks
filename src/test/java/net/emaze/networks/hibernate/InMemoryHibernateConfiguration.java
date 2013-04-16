
package net.emaze.networks.hibernate;

import java.io.IOException;
import java.util.Properties;
import javax.sql.DataSource;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.DefaultComponentSafeNamingStrategy;
import org.hsqldb.jdbcDriver;
import org.springframework.context.annotation.Bean;
import org.springframework.core.io.Resource;
import org.springframework.jdbc.datasource.SimpleDriverDataSource;
import org.springframework.orm.hibernate3.HibernateOperations;
import org.springframework.orm.hibernate3.HibernateTemplate;
import org.springframework.orm.hibernate3.HibernateTransactionManager;
import org.springframework.orm.hibernate3.annotation.AnnotationSessionFactoryBean;
import org.springframework.transaction.PlatformTransactionManager;


public class InMemoryHibernateConfiguration {

    @Bean
    public DataSource dataSource() {
        jdbcDriver driver = new org.hsqldb.jdbcDriver();
        return new SimpleDriverDataSource(driver, "jdbc:hsqldb:mem:blimp-test", "sa", "");
    }

    @Bean
    public PlatformTransactionManager txManager(SessionFactory sessionFatory) {
        final HibernateTransactionManager bean = new HibernateTransactionManager();
        bean.setSessionFactory(sessionFatory);
        return bean;
    }

    @Bean
    public SessionFactory localSessionFactoryBean(final DataSource dataSource) throws IOException, Exception {
        final Properties hibernateProperties = new Properties();
        hibernateProperties.put("hibernate.dialect", "org.hibernate.dialect.HSQLDialect");
        hibernateProperties.put("hibernate.hbm2ddl.auto", "create-drop");
        hibernateProperties.put("hibernate.jdbc.batch_size", "0");
        hibernateProperties.put("hibernate.cache.use_second_level_cache", "false");
        hibernateProperties.put("hibernate.cache.use_query_cache", "false");
        hibernateProperties.put("hibernate.bytecode.use_reflection_optimizer", "true");
        hibernateProperties.put("hibernate.connection.release_mode", "auto");
        hibernateProperties.put("hibernate.show_sql", "false");
        hibernateProperties.put("hibernate.format_sql", "false");
        hibernateProperties.put("hibernate.generate_statistics", "false");
        hibernateProperties.put("hibernate.default_batch_fetch_size", "200");
        final AnnotationSessionFactoryBean factoryBean = new AnnotationSessionFactoryBean();
        factoryBean.setDataSource(dataSource);
        factoryBean.setMappingLocations(new Resource[0]);
        factoryBean.setPackagesToScan(new String[]{"net.emaze.networks.hibernate"});
        factoryBean.setNamingStrategy(new DefaultComponentSafeNamingStrategy());
        factoryBean.setHibernateProperties(hibernateProperties);
        factoryBean.afterPropertiesSet();
        return factoryBean.getObject();
    }

    @Bean
    public HibernateOperations hibernateOperations(SessionFactory sessionFactory) {
        return new HibernateTemplate(sessionFactory);
    }

}
