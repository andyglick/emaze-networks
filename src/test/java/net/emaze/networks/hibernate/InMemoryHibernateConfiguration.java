
package net.emaze.networks.hibernate;

import java.io.IOException;
import java.util.Properties;
import javax.sql.DataSource;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.DefaultComponentSafeNamingStrategy;
import org.hsqldb.jdbc.JDBCDriver;
import org.springframework.context.annotation.Bean;
import org.springframework.core.io.Resource;
import org.springframework.jdbc.datasource.SimpleDriverDataSource;
import org.springframework.orm.hibernate4.HibernateOperations;
import org.springframework.orm.hibernate4.HibernateTemplate;
import org.springframework.orm.hibernate4.HibernateTransactionManager;
import org.springframework.orm.hibernate4.LocalSessionFactoryBean;
import org.springframework.transaction.PlatformTransactionManager;


public class InMemoryHibernateConfiguration {

    @Bean
    public DataSource dataSource() {
        JDBCDriver driver = new JDBCDriver();
        return new SimpleDriverDataSource(driver, "jdbc:hsqldb:mem:emaze-networks-test", "sa", "");
    }

    @Bean
    public PlatformTransactionManager txManager(SessionFactory sessionFatory) {
        final HibernateTransactionManager bean = new HibernateTransactionManager();
        bean.setSessionFactory(sessionFatory);
        return bean;
    }

    @Bean
    public LocalSessionFactoryBean localSessionFactoryBean(final DataSource dataSource) throws IOException, Exception {
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
        final LocalSessionFactoryBean factoryBean = new LocalSessionFactoryBean();
        factoryBean.setDataSource(dataSource);
        factoryBean.setMappingLocations(new Resource[0]);
        factoryBean.setPackagesToScan(new String[]{"net.emaze.networks.hibernate"});
        factoryBean.setNamingStrategy(new DefaultComponentSafeNamingStrategy());
        factoryBean.setHibernateProperties(hibernateProperties);
        return factoryBean;
    }

    @Bean
    public HibernateOperations hibernateOperations(SessionFactory sessionFactory) {
        HibernateTemplate hibernateTemplate = new HibernateTemplate(sessionFactory);
        hibernateTemplate.setCheckWriteOperations(false);
        return hibernateTemplate;
    }

}
